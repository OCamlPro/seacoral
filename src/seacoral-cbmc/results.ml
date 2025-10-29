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

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create "Sc_cbmc.Results" ~doc:"Logs of CBMC results helper"))

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

let add_tests
    (tests : (Sc_values.literal_binding * Ints.t) list)
    res =
  let covered =
    List.fold_left
      (fun acc (_, ids) -> Ints.union acc ids)
      Ints.empty
      tests
  in
  if Ints.subset covered res.covered then
    res
  else if
    Ints.subset res.covered covered &&
    List.length tests < List.length res.test_inputs
  then
    (* The new set of tests covers more than the previous one *)
    { res with
      test_inputs = tests;
      covered;
    }
  else
    { res with
      test_inputs = res.test_inputs @ tests;
      covered = Ints.union covered res.covered }

let add_non_valid_extra_property ac res = {
  res with
  non_valid_extra_properties = ac.DATA.acproperty :: res.non_valid_extra_properties
}

let add_uncoverable (lbl : [< Sc_C.Types.any] Sc_C.Cov_label.t) res =
  {res with uncoverable = Ints.add (Sc_C.Cov_label.id lbl) res.uncoverable}

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

let iter_on_data_stream ?with_print:_ data_stream f =
  Lwt_stream.iter_p
    (function
     | (DATA.ProgramInfo _ | Message _ | CProverStatus _) as d ->
        Log.debug "%a" (Printer.pp_cell ~pp_data:(fun _ -> ignore)) d;
        Lwt.return ()
     | Data data -> f data)
    data_stream

let only_data (cells : 'a DATA.cell list) : 'a list =
  fold_on_data
    (fun acc d -> d :: acc)
    []
    cells

(* Returns the list of tests from a coverage CBMC analysis. *)
let goals_to_test_cases
    ~env ~harness
    output =
  fold_on_data ~with_print:Printer.pp_cbmc_cover_output
    (fun
      (acc : t)
      (cco : DATA.cbmc_cover_output) : t ->
      match cco with
      | Goals goal_details ->
          Log.debug "Goal covered: %i" goal_details.gdgoals_covered;
          acc
      | Tests t ->
          Log.debug "#goals: %i" (List.length t);
          let new_tests =
            List.map (fun i ->
                (* Log.debug "Test: %a" pp_test i; *)
                let test = Harness.test_to_literal harness i in
                let covered = covered_goals_of_test env i in
                test, covered
              ) t
          in
          add_tests new_tests acc
    )
    empty
    output

let goal_stream_to_test_cases ~env ~harness ~stream kont =
  iter_on_data_stream ~with_print:Printer.pp_cbmc_cover_output stream
    (function
     | DATA.Goals goal_details ->
        Log.debug "Goal covered: %i" goal_details.gdgoals_covered;
        Lwt.return ()
     | Tests t ->
        Log.debug "#tests: %i" (List.length t);
        let new_tests =
          List.map (fun i ->
              (* Log.debug "Test: %a" pp_test i; *)
              let test = Harness.test_to_literal harness i in
              let covered = covered_goals_of_test env i in
              test, covered
            ) t
        in
        kont new_tests)

(* First, reads the trace until it reaches an invalid assertion that does not
   correspond to a label and accumulates the labels covered by the trace.
   Then, if there is at least one label covered by the trace before the assertion
   that is not already covered, reads the trace and gathers the initial
   variable assignments. *)
let variable_assigns_from_trace
    (harness : Harness.t)
    (env: simple_label_env)
    (trace : DATA.instruction list) : (Sc_values.literal_binding * Ints.t) option =
  let rec check_trace covered = function
    | [] ->
        (* Log.debug "Trace checked, returning covered labels"; *)
        Some covered
    | (DATA.FailureStep fs) :: tl ->
        begin
          match PropertyMap.find_by_name fs.fsproperty env.proof_objectives with
          | Some (_, lbl) -> (* Assertion reachable *)
              (* Log.debug "Label %i is reachable!" (Sc_C.Cov_label.id lbl); *)
              check_trace (Ints.add (Sc_C.Cov_label.id lbl) covered) tl
          | None -> (* Failure on an assertion! *)
             if
               List.exists
                 (fun DATA.{pname; _} -> fs.fsproperty = pname)
                 env.extra_required_properties;
             then begin
                 Log.debug "Property@ %s@ is@ invalid,@ cannot@ conclude@ on@ the@ \
                            validity@ of@ the@ trace@ after@ that" fs.fsproperty;
                 None
               end
             else begin
                 raise (UNKNOWN_PROPERTY fs.fsproperty)
               end
               
        end
    | _ :: tl -> check_trace covered tl
  in
  match check_trace Ints.empty trace with
  | Some covered ->
     Some ((Harness.trace_to_literal harness trace), covered)
  | _ ->
      None

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
      | None -> (* No interesting trace deduced from  *)
          None
      | Some (test, covered) ->
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

let generic_assertion_check_property (ac: DATA.assertion_check) (cr: t) =
  match ac.acstatus with
  | Success -> cr
  | Failure_
  | Unknown _ ->
      Log.debug "Property@ %s@ is@ invalid:@;ignoring" ac.acproperty;
      add_non_valid_extra_property ac cr

let assertion_check_to_result (harness: Harness.t) (env: simple_label_env)
    (cr: t) (ac : DATA.assertion_check) : t =
  match property_and_lbl_of_ac env ac with
  | None -> (* Not a pclabel *)
      generic_assertion_check_property ac cr
  | Some (_, lbl) when Ints.mem (Sc_C.Cov_label.id lbl) (get_covered cr) ->
      (* Log.debug "Label already handled"; *)
      (* Already treated *)
      cr
  | Some (property, sl) ->
      match ac.acstatus with
      | Success ->
          Log.debug "Label %i is unreachable" (Sc_C.Cov_label.id sl);
          (* We could check now that non_valid_extra_properties is empty or not. *)
          add_uncoverable sl cr
      | Failure_ -> (
          Log.debug "Label %i (%s) is reachable:@ handling counter-example\
                    " (Sc_C.Cov_label.id sl) property.pname;
          (* A counter-example has been found for the label's negation: it is reachable *)
          match treat_counter_example harness env ac.actrace with
          | None -> cr
          | Some v -> add_tests [v] cr
      )
      | Unknown s ->
          Log.debug "Unkwown status (%s) of label %s" s ac.acdescription;
          cr

let assert_data_list_to_test_cases ~env ~harness l : t =
  fold_on_data ~with_print:Printer.pp_cbmc_assert_output
    (fun acc adata ->
      List.fold_left (assertion_check_to_result harness env) acc adata)
    empty
    l

let assert_data_stream_to_test_cases ~env ~harness ~stream kont =
  iter_on_data_stream
    ~with_print:Printer.pp_cbmc_cover_output
    stream
    (Lwt_list.iter_s (fun ac ->
         match property_and_lbl_of_ac env ac with
         | None -> ( (* Not a pclabel *)
           match ac.acstatus with
           | Success -> Lwt.return ()
           | Failure_ | Unknown _ ->
              Log.debug "Property@ %s@ is@ invalid:@;ignoring" ac.acproperty;
              Lwt.return ()
         )
         | Some (property, sl) ->
            match ac.acstatus with
            | Success ->
               let id = Sc_C.Cov_label.id sl in
               Log.debug "Label %i is unreachable" id;
               (* We could check now that non_valid_extra_properties is empty or not. *)
               kont (`Uncov id)
            | Failure_ -> (
              Log.debug "Label %i (%s) is reachable:@ handling counter-example\
                         " (Sc_C.Cov_label.id sl) property.pname;
              (* A counter-example has been found for the label's negation: it is reachable *)
              match treat_counter_example harness env ac.actrace with
              | None -> Lwt.return ()
              | Some r -> kont (`Cov r)
            )
            | Unknown s ->
               Log.debug "Unkwown status (%s) of label %s" s ac.acdescription;
               Lwt.return ())
    )
