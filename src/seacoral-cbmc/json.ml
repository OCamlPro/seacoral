(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Types
open DATA

open Json_encoding

let log_src = Logs.Src.create ~doc:"Logs of CBMC Json helpers" "Sc_cbmc.Json"
module Log = (val (Ez_logs.from_src log_src))

let property_kind_from_string = function
  | "error_label" -> ErrorLabel
  | "assertion" -> Assertion
  | "coverage" -> Coverage
  | s -> Unknown s

module Input = struct

  let malloc_may_fail_encoding : malloc_may_fail encoding =
    conv
      (function
        | CantFail -> false, None, None
        | WithFailAssertion -> true, Some true, None
        | WithNull -> true, None, Some true
        | DefaultFail -> true, None, None
      )
      (fun (may_fail, with_assert, with_null) ->
         if may_fail then
           if with_assert = Some true then WithFailAssertion
           else if with_null = Some true then WithNull
           else DefaultFail
         else CantFail
      )
      (obj3
         (req "malloc-may-fail" bool)
         (opt "malloc-fail-assert" bool)
         (opt "malloc-fail-null" bool)
      )

  let options : json_options encoding = conv
      (fun
        {oarguments; ofunction; ounwind; oproperties;
         oshow_properties; ocover; oerror_label; opointer_check;
         onondet_static; omalloc_may_fail} ->

        let cover_activated = Option.is_some ocover in

        let show_test_suite = cover_activated in
        let unwinding_assertions = not cover_activated in
        (* Todo: add partial-loop when working with cover mode *)
        (oarguments,
         ((ofunction, ounwind, oproperties, oshow_properties, ocover, oerror_label,
           show_test_suite, unwinding_assertions, onondet_static, opointer_check),
          omalloc_may_fail
         )
        )
      )
      (fun
        (oarguments,
         ((ofunction, ounwind, oproperties, oshow_properties,
           ocover, oerror_label, _show_test_suite, _unwind_asser,
           onondet_static, opointer_check), (omalloc_may_fail))
        ) ->
        {oarguments; ofunction; ounwind; oproperties;
         oshow_properties; ocover; oerror_label; onondet_static;
         opointer_check; omalloc_may_fail
        }
      )
      (obj2
         (req "arguments" (list string))
         (req "options" (
             merge_objs begin
               obj10
                 (req "function" string)
                 (opt "unwind" int)
                 (opt "property" (list string))
                 (req "show-properties" bool)
                 (opt "cover" string)
                 (opt "error-label" (list string))
                 (req "show-test-suite" bool)
                 (req "unwinding-assertions" bool)
                 (req "nondet-static" bool)
                 (req "pointer-check" bool)
             end
               malloc_may_fail_encoding
           )
         )
      )

end

module Output = struct
  let source_location : source_location encoding = conv
      (fun
        {slfile; slfunction; slline; slworking_dir} ->
        (slfile, slfunction, slline, slworking_dir))
      (fun
        (slfile, slfunction, slline, slworking_dir) ->
        {slfile; slfunction; slline; slworking_dir})
      (obj4
         (req "file" string)
         (opt "function" string)
         (req "line" string)
         (opt "workingDirectory" string)
      )

  let message : message encoding = conv
      (fun {mtext; mtyp; msource_location} -> (mtext, mtyp, msource_location))
      (fun (mtext, mtyp, msource_location) -> {mtext; mtyp; msource_location})
      (obj3
         (req "messageText" string)
         (req "messageType" string)
         (opt "sourceLocation" source_location)
      )

  let program_info : string encoding = obj1 @@ req "program" string

  let status : status encoding = conv
      (function | Success -> "success" | Failure_ -> "failure" | Unknown s -> s)
      (fun s -> match String.lowercase_ascii s with
         | "success" -> Success | "failure" -> Failure_
         | _ -> Unknown s
      )
      string

  let cell (type a) (enc : a encoding) : a cell encoding = union [
      case
        message
        (function | Message m -> Some m | Data _ | ProgramInfo _ | CProverStatus _ -> None)
        (fun m -> Message m);
      case
        enc
        (function | Data g -> Some g | Message _ | ProgramInfo _ | CProverStatus _ -> None)
        (fun g -> Data g);
      case
        program_info
        (function | ProgramInfo i -> Some i | Message _ | Data _ | CProverStatus _ -> None)
        (fun i -> ProgramInfo i);
      case
        (obj1 @@ req "cProverStatus" status)
        (function | CProverStatus cps -> Some cps | Message _ | Data _ | ProgramInfo _ -> None)
        (fun cps -> CProverStatus cps);
    ]

  let property_class : property_class encoding = conv
      (function
        | ErrorLabel -> "error label"
        | Assertion -> "assertion"
        | Coverage -> "coverage"
        | Unknown s -> s)
      property_kind_from_string
      string

  let property : property encoding = conv
      (fun
        {pclass; pdescription; pexpression; pname; psource_location;} ->
        (pclass, pdescription, pexpression, pname, psource_location)
      )
      (fun
        (pclass, pdescription, pexpression, pname, psource_location) ->
        {pclass; pdescription; pexpression; pname; psource_location}
      )
      (obj5
         (req "class" property_class)
         (req "description" string)
         (req "expression" string)
         (req "name" string)
         (req "sourceLocation" source_location)
      )

  let properties : property list encoding =
    obj1 (req "properties" (list property))

  let property_data : property list cell list encoding = list (cell properties)

  let string_or_bool : string encoding = union [
      case string (fun s -> Some s) (fun s -> s);
      case bool bool_of_string_opt string_of_bool;
    ]

  let known_value_case : base_value case =
    case (
        obj5
          (opt "binary" string)
          (req "data" string_or_bool)
          (req "name" string)
          (opt "type" string)
          (opt "width" int)
      )
      (function | Value {vbinary; vdata; vname; vtype; vwidth} -> Some (vbinary, vdata, vname, vtype, vwidth) | Unknown _ -> None)
      (fun (vbinary, vdata, vname, vtype, vwidth) -> Value {vbinary; vdata; vname; vtype; vwidth})
    

  let unknown_value_case : base_value case =
    case (
        obj2
          (req "name" string)
          (opt "type" string)
      )
      (function | Unknown (n, t) -> Some (n, t) | Value _ -> None)
      (fun (str, t) -> Unknown (str, t))

  let base_value : base_value encoding =
    union [
      known_value_case;
      unknown_value_case;
    ]

  let _structure_field value_encoding : structure_field encoding = conv
      (fun {sfname; sfvalue} -> (sfname, sfvalue))
      (fun (sfname, sfvalue) -> {sfname; sfvalue})
      (obj2
         (req "name" string)
         (req "value" value_encoding)
      )

  let _structured value_encoding : structured encoding = conv
      (fun {smembers; sname} -> (smembers, sname))
      (fun (smembers, sname) -> {smembers; sname})
      (obj2
         (req "members" (list (_structure_field value_encoding)))
         (req "name" string)
      )

  let array_element_encoding value_encoding =
    obj2 (req "index" int) (req "value" value_encoding)

  let array_encoding value_encoding = conv
      (fun l -> l, "array")
      (fun (l, array) -> assert (array = "array"); l)
      (obj2
         (req "elements" (list @@ array_element_encoding value_encoding))
         (req "name" string)
      )

  let union_encoding value_encoding = conv
      (fun v -> v, "union")
      (fun (v, union) -> assert (union = "union"); v)
      (obj2
         (req "member" (_structure_field value_encoding))
         (req "name" string)
      )

  let value : value encoding = mu "value"
      (fun value_encoding ->
         union [
           case
             base_value
             (function | Base v -> Some v | Structured _ | Array _ | Union _ -> None)
             (fun v -> Base v);
           case
             (_structured value_encoding)
             (function | Structured s -> Some s | _ -> None)
             (fun s -> Structured s);
           case
             (array_encoding value_encoding)
             (function | Array l -> Some l | _ -> None)
             (fun l -> Array l);
           case
             (union_encoding value_encoding)
             (function | Union v -> Some v | _ -> None)
             (fun v -> Union v)
         ]
      )

  let structure_field = _structure_field value
  let structured = _structured value

  let input = conv
      (fun {iid; ivalue} -> (iid, ivalue))
      (fun (iid, ivalue) -> {iid; ivalue})
      (obj2
         (req "id" string)
         (req "value" value)
      )


  let tests = conv
      (fun {tcovered_goals; tinputs} -> tcovered_goals, tinputs)
      (fun (tcovered_goals, tinputs) -> {tcovered_goals; tinputs})
      (obj2
         (req "coveredGoals" (list string))
         (req "inputs" (list input))
      )

  let goal = conv
      (fun
        {gdescription; ggoal; gsource_location; gstatus} ->
        (gdescription, ggoal, gsource_location, gstatus)
      )
      (fun
        (gdescription, ggoal, gsource_location, gstatus) ->
        {gdescription; ggoal; gsource_location; gstatus}
      )
      (obj4
         (req "description" string)
         (req "goal" string)
         (req "sourceLocation" source_location)
         (req "status" status)
      )

  let goals_details = conv
      (fun
        {gdgoals; gdgoals_covered; gdtotal_goals} ->
        (gdgoals, gdgoals_covered, gdtotal_goals)
      )
      (fun
        (gdgoals, gdgoals_covered, gdtotal_goals) ->
        {gdgoals; gdgoals_covered; gdtotal_goals}
      )
      (obj3
         (req "goals" (list goal))
         (req "goalsCovered" int)
         (req "totalGoals" int)
      )

  let cbmc_cover_output = union [
      case
        goals_details
        (function | Goals gd -> Some gd | Tests _ -> None)
        (fun gd -> Goals gd);
      case
        (obj1 (req "tests" (list tests)))
        (function | Goals _ -> None | Tests t -> Some t)
        (fun t -> Tests t);
    ]

  let cover_analysis_output = list @@ cell cbmc_cover_output

  let function_ = conv
      (fun
        {fdisplay_name; fidentifier; fsource_location} ->
        (fdisplay_name, fidentifier, fsource_location)
      )
      (fun
        (fdisplay_name, fidentifier, fsource_location) ->
        {fdisplay_name; fidentifier; fsource_location}
      )
      (obj3
         (req "displayName" string)
         (req "identifier" string)
         (req "sourceLocation" source_location)
      )

  let common_instr_info =
    obj4
      (req "hidden" bool)
      (req "internal" bool)
      (req "stepType" string)
      (req "thread" int)

  let function_info = conv
      (fun
        {fifunction; fisource_location; fihidden; fiinternal; fistep_type; fithread} ->
        ((fifunction, fisource_location), (fihidden, fiinternal, fistep_type, fithread))
      )
      (fun
        ((fifunction, fisource_location), (fihidden, fiinternal, fistep_type, fithread)) ->
        {fifunction; fisource_location; fihidden; fiinternal; fistep_type; fithread}
      )
      (
        merge_objs
          (obj2
             (req "function" function_)
             (opt "sourceLocation" source_location)
          )
          common_instr_info
      )

  let assignment = conv
      (fun
        {
          atype; ahidden; ainternal;
          alhs; amode; asource_location;
          astep_type; athread; avalue;
          areason
        } ->
        (
          (ahidden, ainternal, astep_type, athread),
          (atype, alhs, amode, asource_location, avalue, areason)
        )
      )
      (
        fun
          (
            (ahidden, ainternal, astep_type, athread),
            (atype, alhs, amode, asource_location, avalue, areason)
          ) ->
          {
            atype; ahidden; ainternal;
            alhs; amode; asource_location;
            astep_type; athread; avalue; areason
          }
      )
      (merge_objs common_instr_info
         (obj6
            (req "assignmentType" string)
            (req "lhs" string)
            (req "mode" string)
            (req "sourceLocation" source_location)
            (req "value" value)
            (opt "reason" string)
         )
      )

  let location_only = conv
      (fun
        {lohidden; losource_location; lostep_type; lothread} ->
        (lohidden, losource_location, lostep_type, lothread)
      )
      (fun
        (lohidden, losource_location, lostep_type, lothread) ->
        {lohidden; losource_location; lostep_type; lothread}
      )
      (obj4
         (req "hidden" bool)
         (req "sourceLocation" source_location)
         (req "stepType" string)
         (req "thread" int)
      )

  let input_step = conv
      (fun {ihidden; iinputID; iinternal; imode; isource_location; istep_type; ithread; ivalues} ->
         (ihidden, iinternal, istep_type, ithread), (iinputID, imode, isource_location, ivalues)
      )
      (fun ((ihidden, iinternal, istep_type, ithread), (iinputID, imode, isource_location, ivalues)) ->
         {ihidden; iinputID; iinternal; imode; isource_location; istep_type; ithread; ivalues}
      )
      (merge_objs common_instr_info
         (obj4
            (req "inputID" string)
            (req "mode" string)
            (req "sourceLocation" source_location)
            (req "values" (list value))
         )
      )

  let failure_step = conv
      (
        fun
          {fshidden; fsinternal; fsproperty; fsreason; fssource_location; fsstep_type; fsthread} ->
          ((fshidden, fsinternal, fsstep_type, fsthread), (fsproperty, fsreason, fssource_location))
      )
      (
        fun
          ((fshidden, fsinternal, fsstep_type, fsthread), (fsproperty, fsreason, fssource_location)) ->
          {fshidden; fsinternal; fsproperty; fsreason; fssource_location; fsstep_type; fsthread}
      )
      (
        merge_objs common_instr_info
          (obj3
             (req "property" string)
             (req "reason" string)
             (req "sourceLocation" source_location)
          )
      )

  let output_step = conv
      (fun {ohidden; ointernal; ostep_type; othread; omode; oid; oloc; ovalues} ->
         ((ohidden, ointernal, ostep_type, othread),
          (omode, oid, oloc, ovalues)))
      (fun ((ohidden, ointernal, ostep_type, othread),
            (omode, oid, oloc, ovalues)) ->
        {ohidden; ointernal; ostep_type; othread; omode; oid; oloc; ovalues})
      (merge_objs common_instr_info
         (obj4
            (req "mode" string)
            (req "outputID" string)
            (req "sourceLocation" source_location)
            (req "values" (list value))))

  let instruction = union [
      case function_info
        (function Function f -> Some f | _ -> None)
        (fun f -> Function f);
      case assignment
        (function Assignment a -> Some a | _ -> None)
        (fun a -> Assignment a);
      case location_only
        (function Location a -> Some a | _ -> None)
        (fun l -> Location l);
      case failure_step
        (function FailureStep fs -> Some fs | _ -> None)
        (fun fs -> FailureStep fs);
      case input_step
        (function Input i -> Some i | _ -> None)
        (fun i -> Input i);
      case output_step
        (function Output o -> Some o | _ -> None)
        (fun o -> Output o);
    ]

  let assertion_check = conv
      (fun
        {acdescription; acproperty; acsource_location; acstatus; actrace} ->
        (acdescription, acproperty, acsource_location, acstatus, actrace)
      )
      (fun
        (acdescription, acproperty, acsource_location, acstatus, actrace) ->
        {acdescription; acproperty; acsource_location; acstatus; actrace}
      )
      (obj5
         (req "description" string)
         (req "property" string)
         (opt "sourceLocation" source_location)
         (req "status" status)
         (opt "trace" (list instruction))
      )

  let assert_analysis_result = obj1 (req "result" (list assertion_check))

  let assert_analysis_output = list @@ cell assert_analysis_result
end

let log_exn exn =
  let rec __print_exn indent exn =
    match exn with
    | Json_encoding.Cannot_destruct (path, exn) ->
        Log.err "%s%a" indent (Json_query.print_path_as_json_path ~wildcards:true) path;
        __print_exn indent exn
    | Json_encoding.No_case_matched el ->
        List.iter (__print_exn ("  " ^ indent)) el;
    | Json_encoding.Missing_field d ->
        Log.err "%sMissing field \"%s\"" indent d
    | _ ->
        Log.err "%sError %s" indent (Printexc.to_string exn)
  in
  __print_exn "" exn

let options options =
  let json = Json_encoding.construct Input.options options in
  Yojson.Safe.to_string @@ Json_repr.to_yojson json

let read_cbmc_output encoding str =
  let js =
    try
      let yoj = Yojson.Safe.from_string str in
      Json_repr.from_yojson yoj
    with
    | Yojson.Json_error _ as exn -> begin
        (* When CBMC fails, it outputs a list of JSON values with a missing ']'.
           We try here to rebuild the correct JSON. *)
        try
          let yoj = Yojson.Safe.from_string (str ^ "]") in
          Json_repr.from_yojson yoj
        with
        | Yojson.Json_error _ ->
           (* We tried fixing the JSON and failed: printing the old exception. *)
           log_exn exn;
           raise (FAILED_JSON_PARSING {exn; json = str})
      end
  in
  try Json_encoding.destruct encoding js with
  | exn ->
     log_exn exn;
     raise (FAILED_JSON_DESTRUCT {exn; json = str})

