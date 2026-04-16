(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
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

type test_type =
  | Labels of Basics.Ints.t
  | RTE of DATA.assertion_check
  | OracleFail of DATA.assertion_check

type coverable = [ `Test of Sc_values.literal_binding * test_type ]

type res = [
    coverable
  | `Uncov of int
  | `Extra of DATA.assertion_check
  ]

type extra_properties = {
  valid: string list;
  invalid: string list;
  unknown: string list;
}

type t = {
  test_inputs: (Sc_values.literal_binding * test_type) list;
  (** The test inputs and the labels they cover *)
  covered: Ints.t;
  (** All the covered labels *)
  uncoverable : Ints.t;
  (** The set of satisfied properties *)
  extra_properties: extra_properties;
  (** The set of extra properties. *)
}

let empty = {
  test_inputs = [];
  covered = Ints.empty;
  uncoverable = Ints.empty;
  extra_properties = {valid = []; invalid = []; unknown = []};
}

let add_test ((t, cov) as test) res =
  let rec loop prev_tests = function
    | [] -> test :: List.rev prev_tests
    | ((t', _) as test') :: tl ->
       if t = t'
       then (List.rev prev_tests) @ tl
       else loop (test' :: prev_tests) tl
  in
  match cov with
  | Labels cov ->
     {
       res with
       test_inputs = loop [] res.test_inputs
     ; covered = Ints.union res.covered cov}
  | RTE _ | OracleFail _ ->
     {res with test_inputs = loop [] res.test_inputs}

let add_valid_extra_prop pname e = {e with valid = pname :: e.valid}
let add_invalid_extra_prop pname e = {e with invalid = pname :: e.invalid}
let add_unknown_extra_prop pname e = {e with unknown = pname :: e.unknown}

let add_extra_ac ac res =
  let pname = ac.DATA.acproperty in
  match ac.DATA.acstatus with
  | Success -> {res with extra_properties = add_valid_extra_prop pname res.extra_properties}
  | Failure_ -> {res with extra_properties = add_invalid_extra_prop pname res.extra_properties}
  | Unknown _ -> {res with extra_properties = add_unknown_extra_prop pname res.extra_properties}

let add_uncoverable i res =
  {res with uncoverable = Ints.add i res.uncoverable}

let get_tests c = c.test_inputs

let get_covered c = c.covered

let get_uncoverable c =
  if c.extra_properties.invalid = [] && c.extra_properties.unknown = []
  then c.uncoverable
  else Ints.empty

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
         List.map (fun (t, c) -> `Test (t, Labels c)) new_tests
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
                         validity@ of@ the@ trace@ after@ that.@ Keeping@ the@ \
                         test just in case." fs.fsproperty;
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

let property_and_lbl_of_ac (env: simple_label_env) (ac : DATA.assertion_check) =
  if ac.DATA.acdescription = Harness.oracle_property_identifier then
    `Oracle
  else
    match PropertyMap.find_by_name ac.acproperty env.proof_objectives with
    | Some (p, l) -> `Label (p, l)
    | None -> `CBMC_internal

let treat_counter_example
    ~for_rte
    (harness : Harness.t)
    (env: simple_label_env)
    ac =
  match ac.DATA.actrace with
  | None ->
     Log.warn "@[<2>Property@ %s@ has@ no@ trace@ with@ it.@]" ac.acproperty;
     None
  | Some trace ->
      match variable_assigns_from_trace harness env trace with
      | (test, covered) ->
          if not for_rte then
            Log.debug "@[<2>Test@ covering@ labels@ %a:@;%a@]"
              Ints.print covered
              Sc_values.pp_literal_binding test
          else
            Log.debug "@[<2>Test@ raising@ an@ RTE:@;%a@]"
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

let assert_data_stream_to_test_cases_stream ~env ~harness ~stream =
  map_data_stream
    stream
    (fun l ->
      List.fold_left
        (fun acc ac ->
          match property_and_lbl_of_ac env ac, ac.acstatus with
          | `Oracle, Failure_ -> begin
             (* A test breaking the oracle! *)
             Log.debug "@[<2>Found a counter example for the oracle!@]";
             match treat_counter_example ~for_rte:true harness env ac with
             | None -> acc
             | Some (test, _) -> acc @ [ `Test (test, OracleFail ac) ]
            end
          | `Oracle, (Unknown _ | Success) -> acc
          | `CBMC_internal, Failure_ -> begin
             (* A RTE! *)
             Log.debug "@[<2>Property@ %s@ is@ an@ rte!@]" ac.acproperty;
             match treat_counter_example ~for_rte:true harness env ac with
             | None -> acc
             | Some (test, _) -> acc @ [ `Test (test, RTE ac) ]
            end
          | `CBMC_internal, Unknown _ -> begin
             (* A possible RTE? *)
              Log.debug "@[<2>Property@ %s@ is@ a@ possible@ rte,@ no@ \
                         counter-example@ found.@]" ac.acproperty;
             acc @ [`Extra ac]
            end
          | `CBMC_internal, Success -> (* For sure, not a RTE *)
             acc @ [`Extra ac]
          | `Label (_, sl), Success -> begin
              let id = Sc_C.Cov_label.id sl in
              Log.debug "Label@ %i@ is@ unreachable" id;
              (* We could check now that non_valid_extra_properties is empty
                 or not. *)
              acc @ [`Uncov id]
            end
          | `Label (property, sl), Failure_ -> begin
             Log.debug "Label@ %i@ (%s)@ may@ be@ reachable:@ handling@ \
                        counter-example\
                        " (Sc_C.Cov_label.id sl) property.pname;
             (* A counter-example has been found for the label's negation: it
                  is reachable *)
             match treat_counter_example ~for_rte:false harness env ac with
             | None -> acc
             | Some (test, c) -> acc @ [ `Test (test, Labels c) ]
            end
          | `Label _, Unknown s ->
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
      | `Test t -> add_test t res
      | `Uncov i -> add_uncoverable i res
      | `Extra ac -> add_extra_ac ac res)
    empty
    l
