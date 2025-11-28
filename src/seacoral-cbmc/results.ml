(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Basics
open Types
open Lwt.Syntax

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create "Sc_cbmc.Results" ~doc:"Logs of CBMC results helper"))

type coverable = [ `Cov of Sc_values.literal_binding * Basics.Ints.t ]

type res = [
    coverable
  | `Uncov of int
  | `NonValidExtra of string
  ]

type t = {
  test_inputs: (Sc_values.literal_binding * Ints.t) list;
  (** The test inputs and the labels they cover *)
  covered: Ints.t;
  (** All the covered labels *)
  uncoverable : Ints.t;
  (** The set of satisfied properties *)
  non_valid_extra_properties: string list;
  (** The set of unknown and invalid extra properties. *)
}

let empty = {
  test_inputs = [];
  covered = Ints.empty;
  uncoverable = Ints.empty;
  non_valid_extra_properties = [];
}

let add_test (t, cov) res =
  let rec loop prev_tests = function
    | [] -> (t, cov) :: List.rev prev_tests
    | ((t', cov') as r) :: tl ->
       if t = t' then
         List.rev prev_tests @ (t, Ints.union cov cov') :: tl
       else if Ints.subset cov' cov then
         (* Removing test that covers less *)
         List.rev prev_tests @ (t, cov) :: tl
       else
         loop (r :: prev_tests) tl
  in
  if Ints.subset cov res.covered
  then res
  else {
      res with
      test_inputs = loop [] res.test_inputs
    ; covered = Ints.union res.covered cov
    }

let add_tests = List.fold_right add_test

let add_non_valid_extra_property pname res = {
  res with
  non_valid_extra_properties = pname :: res.non_valid_extra_properties
}

let add_non_valid_extra_property_ac ac res =
  add_non_valid_extra_property ac.DATA.acproperty res

let add_uncoverable i res =
  {res with uncoverable = Ints.add i res.uncoverable}

let add_uncoverable_label (lbl : [< Sc_C.Types.any] Sc_C.Cov_label.t) res =
  add_uncoverable (Sc_C.Cov_label.id lbl) res

let get_tests c = c.test_inputs

let get_covered c = c.covered

let get_uncoverable c =
  if c.non_valid_extra_properties = [] then c.uncoverable else Ints.empty

(* Returns the list of labels covered by the test in argument. *)
let covered_goals_of_test env DATA.{ tcovered_goals; _ } : Ints.t =
  (* TODO: Should we check the function name as well? *)
  List.to_seq tcovered_goals |> Seq.filter_map (fun cg ->
      match PropertyMap.find_by_name cg env.proof_objectives with
      | None ->
          (* It should be an already proven property then *)
          begin match PropertyMap.find_by_name cg env.already_proven with
            | None ->
                Log.err "Goal %s not found in environment" cg;
                Log.debug
                  "Studied properties:@.%a"
                  (PropertyMap.print ~check_equal:cg) env.proof_objectives;
                Log.debug
                  "Already known properties:@.%a"
                  (PropertyMap.print ~check_equal:cg) env.already_proven;
                raise Not_found
            | Some _ ->
                Log.debug "Goal %s was already proven" cg;
                None
          end
      | Some (_, lbl) -> Some (Sc_C.Cov_label.id lbl))
  |> Ints.of_seq

let fold_on_data ?with_print:_ f acc data_list =
  (* let pp = *)
  (*   match with_print with *)
  (*   | None -> (fun _ _ -> ()) *)
  (*   | Some pp_data -> Printer.pp_cell ~pp_data *)
  (* in *)
  List.fold_left
    (fun acc v ->
       (* Log.debug "%a" pp v; *)
       match v with
       | DATA.ProgramInfo _ -> acc
       | Message _ -> acc
       | CProverStatus _s -> acc (* TODO *)
       | Data data -> f acc data
    )
    acc
    data_list

let fold_on_data_stream ?with_print:_ data_stream f acc =
  Lwt_stream.fold_s
    (fun v acc -> match v with
     | (DATA.ProgramInfo _ | Message _ | CProverStatus _) as d ->
        Log.debug "%a" (Printer.pp_cell ~pp_data:(fun _ -> ignore)) d;
        Lwt.return acc
     | Data data -> f acc data)
    data_stream
    acc

let map_data_stream ?with_print:_ data_stream f =
  Lwt_stream.map_list
    (fun v -> match v with
     | (DATA.ProgramInfo _ | Message _ | CProverStatus _) as d ->
        Log.debug "%a" (Printer.pp_cell ~pp_data:(fun _ -> ignore)) d;
        []
     | Data data -> f data )
    data_stream

let only_data (cells : 'a DATA.cell list) : 'a list =
  fold_on_data
    (fun acc d -> d :: acc)
    []
    cells

let goal_stream_to_test_cases ~env ~harness ~stream kont =
  fold_on_data_stream
    ~with_print:Printer.pp_cbmc_cover_output
    stream
    (fun acc -> function
      | DATA.Goals goal_details ->
         Log.debug "Goal covered: %i" goal_details.gdgoals_covered;
         Lwt.return acc
      | Tests t ->
         Log.debug "#tests: %i" (List.length t);
         let new_tests =
           List.map (fun i ->
               let test = Harness.test_to_literal harness i in
               let covered = covered_goals_of_test env i in
               test, covered
             ) t
         in
         let* () = kont new_tests in
         Lwt.return @@ add_tests new_tests acc
    )
    empty

let goal_stream_to_test_cases_stream ~env ~harness ~stream =
  map_data_stream
    stream
    (function
      | DATA.Goals goal_details ->
         Log.debug "Goal covered: %i" goal_details.gdgoals_covered;
         []
      | Tests t ->
         Log.debug "#tests: %i" (List.length t);
         let new_tests =
           List.map (fun i ->
               let test = Harness.test_to_literal harness i in
               let covered = covered_goals_of_test env i in
               test, covered
             ) t
         in
         List.map (fun (t, c) -> `Cov (t, c)) new_tests
    )  

(* First, reads the trace until it reaches an invalid assertion that does not
   correspond to a label and accumulates the labels covered by the trace.
   Then, if there is at least one label covered by the trace before the assertion
   that is not already covered, reads the trace and gathers the initial
   variable assignments. *)
let variable_assigns_from_trace
    (harness : Harness.t)
    (env: simple_label_env)
    (trace : DATA.instruction list) : (Sc_values.literal_binding * Ints.t) =
  let rec check_trace ~invalid covered = function
    | [] ->
        (* Log.debug "Trace checked, returning covered labels"; *)
        covered
    | (DATA.FailureStep fs) :: tl ->
        begin
          match PropertyMap.find_by_name fs.fsproperty env.proof_objectives with
          | Some (_, lbl) -> (* Assertion reachable *)
              (* Log.debug "Label %i is reachable!" (Sc_C.Cov_label.id lbl); *)
             check_trace ~invalid (Ints.add (Sc_C.Cov_label.id lbl) covered) tl
          | None -> begin (* Failure on an assertion! *)
             match PropertyMap.find_by_name fs.fsproperty env.already_proven with
             | Some _ -> check_trace ~invalid:true covered tl
             | None -> (* Not proven yet *)
                if
                  List.exists
                    (fun DATA.{pname; _} -> fs.fsproperty = pname)
                    env.extra_required_properties;
                then begin
                    if not invalid then
                      Log.debug
                        "Property@ %s@ is@ invalid,@ cannot@ conclude@ on@ the@ \
                         validity@ of@ the@ trace@ after@ that" fs.fsproperty;
                    check_trace ~invalid:true covered tl
                  end
                else
                  raise (UNKNOWN_PROPERTY fs.fsproperty)
            end
        end
    | _ :: tl -> check_trace ~invalid covered tl
  in
  let covered = check_trace ~invalid:false Ints.empty trace in
  Harness.trace_to_literal harness trace, covered

let property_and_lbl_of_ac (env: simple_label_env) (ac : DATA.assertion_check)
  : (DATA.property * Sc_C.Cov_label.simple) option =
  (* Log.debug "Property and label of assertion check %s" ac.acproperty; *)
  PropertyMap.find_by_name ac.acproperty env.proof_objectives

let treat_counter_example
    (harness : Harness.t)
    (env: simple_label_env)
    actrace =
  (* Log.debug "Handling counter example %s" ac.acdescription; *)
  match actrace with
  | None -> (* invalid_trace ~trace:[] ~prop:ac.acproperty ~reason:"Counter example without a trace" *)
      failwith "TODO: invalid_trace"
  | Some trace ->
      match variable_assigns_from_trace harness env trace with
      | (test, covered) ->
          Log.debug "@[<2>Test@ covering@ labels@ %a:@;%a@]"
            Ints.print covered
            Sc_values.pp_literal_binding test;
          Some (test, covered)
      | exception (UNKNOWN_PROPERTY pname) ->
         (* We reached a property that was not registered as such previously.
            Discarding the counter example for safety.
            TODO: we could check whether the validator manages to do something
            with it, in which case we would not have to raise this exception *)
         Log.err
           "Property@ %s@ is@ unknown. Discarding the counter-example"
           pname;
         None

let assert_data_stream_to_test_cases ~env ~harness ~stream kont =
  fold_on_data_stream
    ~with_print:Printer.pp_cbmc_cover_output
    stream
    (fun (acc : t) l ->
      Lwt_list.fold_left_s
        (fun (acc : t) ac ->
          match property_and_lbl_of_ac env ac with
          | None -> ( (* Not a pclabel *)
            match ac.acstatus with
            | Success -> Lwt.return acc
            | Failure_ ->
               Log.debug "Property@ %s@ is@ invalid:@;ignoring" ac.acproperty;
               Lwt.return @@ add_non_valid_extra_property_ac ac acc
            | Unknown _ ->
               (* Log.debug "Property@ %s@ status@ is@ unknown:@;ignoring" ac.acproperty; *)
               Lwt.return @@ add_non_valid_extra_property_ac ac acc
          )
          | Some (property, sl) ->
             match ac.acstatus with
             | Success ->
                let id = Sc_C.Cov_label.id sl in
                Log.debug "Label %i is unreachable" id;
                (* We could check now that non_valid_extra_properties is empty or not. *)
                let* () = kont (`Uncov id) in
                Lwt.return @@ add_uncoverable_label sl acc         
             | Failure_ -> (
               Log.debug "Label %i (%s) may be reachable:@ handling counter-example\
                          " (Sc_C.Cov_label.id sl) property.pname;
               (* A counter-example has been found for the label's negation: it is reachable *)
               match treat_counter_example harness env ac.actrace with
               | None -> Lwt.return acc
               | Some ((test, _) as r) ->
                  let* () =
                    if not (List.exists (fun (t, _) -> t = test) acc.test_inputs) then
                      kont (`Cov r)
                    else (
                      Log.debug "Test already generated, not replaying it";
                      Lwt.return ()
                    )
                  in
                  Lwt.return @@ add_test r acc
             )
             | Unknown s ->
                Log.debug "Unkwown status (%s) of label %s" s ac.acdescription;
                Lwt.return acc)
        acc
        l
    )
    empty

let assert_data_stream_to_test_cases_stream ~env ~harness ~stream =
  map_data_stream
    stream
    (fun l ->
      List.fold_left
        (fun acc ac ->
          match property_and_lbl_of_ac env ac with
          | None -> ( (* Not a pclabel *)
            match ac.acstatus with
            | Success -> acc
            | Unknown _ ->
               Log.debug
                 "Property@ %s@ status@ is@ unknown:@;ignoring"
                 ac.acproperty;
               acc @ [`NonValidExtra ac.acproperty]
            | Failure_ ->
               Log.debug "Property@ %s@ is@ invalid:@;ignoring" ac.acproperty;
               acc @ [`NonValidExtra ac.acproperty]
          )
          | Some (property, sl) ->
             match ac.acstatus with
             | Success ->
                let id = Sc_C.Cov_label.id sl in
                Log.debug "Label@ %i@ is@ unreachable" id;
                (* We could check now that non_valid_extra_properties is empty
                   or not. *)
                acc @ [`Uncov id]      
             | Failure_ -> (
               Log.debug "Label@ %i@ (%s)@ may@ be@ reachable:@ handling@ \
                          counter-example\
                          " (Sc_C.Cov_label.id sl) property.pname;
               (* A counter-example has been found for the label's negation: it
                  is reachable *)
               match treat_counter_example harness env ac.actrace with
               | None -> acc
               | Some (test, c) -> acc @ [ `Cov (test, c) ]
             )
             | Unknown s ->
                Log.debug "Unkwown@ status@ (%s)@ of@ label@ %s\
                           " s ac.acdescription;
                acc
        )
        []
        l
    )

let summing_up l =
  List.fold_left
    (fun (res : t) -> function
      | `Cov (t, c) -> add_test (t, c) res
      | `Uncov i -> add_uncoverable i res
      | `NonValidExtra s -> add_non_valid_extra_property s res)
    empty
    l
