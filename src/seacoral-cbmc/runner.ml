(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Json interface for CBMC. *)

open Types
open DATA

open Lwt.Syntax

type 'a process_result = 'a

type 'a cbmc_run =
  store:Sc_store.t ->
  runner_options:runner_options ->
  entrypoint:string ->
  files:[ `C ] Sc_sys.File.t list ->
  OPTIONS.t ->
  ('a Lwt_stream.t * (unit -> unit Lwt.t)) Lwt.t

(* --- *)

let log_src = Logs.Src.create ~doc:"Logs of CBMC caller" "Sc_cbmc.Runner"
module Log = (val (Ez_logs.from_src log_src))

type _ exec_kind =
  | GetProperties : property list cell exec_kind
  | GetCoverObjectives : property list cell exec_kind
  | GetCLabels : Sc_C.Cov_label.simple list -> property list cell exec_kind
  | CoverAnalysis : [`simple] analysis_env -> cbmc_cover_output cell exec_kind
  | AssertAnalysis : [`simple] analysis_env -> cbmc_assert_output cell exec_kind
  | CLabelAnalysis : [`simple] analysis_env -> cbmc_assert_output cell exec_kind

let pp_execution_kind (type k) : k exec_kind Fmt.t = fun ppf e ->
  Fmt.string ppf @@ match e with
  | GetProperties -> "get-properties"
  | GetCoverObjectives -> "get-objectives"
  | GetCLabels _ -> "get-clabels"
  | CoverAnalysis _ -> "cover-analysis"
  | AssertAnalysis _ -> "assert-analysis"
  | CLabelAnalysis _ -> "clabels-analysis"

let _source_location_to_location (sloc : source_location) : Sc_C.Types.location =
  try { loc_line = int_of_string sloc.slline;
        loc_file = Filename.basename sloc.slfile }
  with Failure s -> Fmt.failwith "Source location to location failed: %s" s

let property_belongs_to_file ~file prop =
  Filename.basename prop.psource_location.slfile =
  Sc_sys.File.basename file

let property_kind_matches_mode ~mode ~kind =
  match mode with
  | OPTIONS.Cover -> kind = "coverage"
  | Assert -> kind = "assertion"
  | CLabel -> kind = "error_label"

let info_from_property_name prop =
    match String.split_on_char '.' prop.pname with
    | [fname; kind; id] -> fname, kind, int_of_string id
    | _ -> Fmt.failwith "CBMC property name %S expected to be of the form [entrypoint].[kind].[id]." prop.pname

let id_from_assert_property_descr prop =
  match String.split_on_char '.' prop.pdescription with
  | [ "__sc_assertion"; id ] -> Some (int_of_string id)
  | _ -> None

let info_from_property
      ~harness_file (* File where the labels are put *)
      ~labelized_file (* Labelized file *)
      ~entrypoint (* Name of the harness entrypoint (not the analyzed function) *)
      ~mode
      prop =
  if property_belongs_to_file ~file:labelized_file prop then
    `ExtraProperty
  else if (property_belongs_to_file ~file:harness_file prop) then
    let fname, kind, id = info_from_property_name prop in
    if fname <> entrypoint || not (property_kind_matches_mode ~mode ~kind) then
      `ExtraProperty
    else if mode = Assert then begin
        (* Sometimes, seacoral adds extra assertions, which makes the [id] from the
           prop.name not the correct one. *)
        match id_from_assert_property_descr prop with
        | None -> `DiscardProperty
        | Some id -> `Label id
      end
    else `Label id
  else
    `DiscardProperty

let empty_env =
  { extra_required_properties = [];
    proof_objectives = PropertyMap.empty;
    already_proven = PropertyMap.empty }

(* Takes the list of properties returned by cbmc with the option --show-properties and
   returns the associated proof objectives (the labels to cover). *)
let uncovered_properties
    ~mode
    ~harness_file
    ~labelized_file
    ~(cbmc_props : property list)
    ~(already_decided : Basics.Ints.t)
    ~labels
    ~entrypoint
  : [`simple] analysis_env =
  (* Sorting labels in a map for quick search *)
  let lbl_map =
    Basics.IntMap.of_seq @@                     (* of_list requires OCaml≥5.1 *)
    Seq.map (fun l -> Sc_C.Cov_label.id l, l) @@
    List.to_seq labels
  in
  let env =
    List.fold_left begin fun env prop ->
      Log.debug "Property %a" Printer.pp_property prop;
      match info_from_property ~harness_file ~labelized_file ~entrypoint ~mode prop with
      | `DiscardProperty -> Log.debug "Discarding@ property@ %s" prop.pname; env
      | `ExtraProperty ->
         Log.debug "Extra property";
         { env with
           extra_required_properties = prop :: env.extra_required_properties }
      | `Label lbl_id ->
         match Basics.IntMap.find_opt lbl_id lbl_map with
         | None ->                                              (* not a label *)
            Log.warn "Property %s read as a label, but not in the label map. \
                      This should not occur." prop.pname;
            { env with
              extra_required_properties = prop :: env.extra_required_properties }
         | Some lbl when
                Sc_C.Cov_label.is_unknown lbl &&
                  not (Basics.Ints.mem lbl_id already_decided) ->(* unknown status *)
            Log.debug "Label with unknown status: %i" (Sc_C.Cov_label.id lbl);
            { env with
              proof_objectives = PropertyMap.add prop lbl env.proof_objectives }
         | Some lbl ->                                          (* known status *)
            Log.debug "Label with status %s (%b)"
              (Sc_C.Cov_label.(show_status @@ status lbl))  (Basics.Ints.mem lbl_id already_decided);
            { env with
              already_proven = PropertyMap.add prop lbl env.already_proven }
    end empty_env cbmc_props
  in
  SimpleLabelEnv env


(* [treat_cbmc_output_stream encoding stream] reads the stream contents,
   expecting it to be an array of json objects (the output format of CBMC), and
   returns the corresponding stream of json objects decoded according to
   [encoding]. *)
let decode_cbmc_output_stream encoding stream =
  let out, emit = Lwt_stream.create () in
  Lwt.async begin fun () ->
    let json = Buffer.create 42 in
    let parenthesis_depth = ref 0
    and in_quotes = ref false
    and junk = ref false in
    Lwt.map ignore @@
    Lwt_stream.fold begin fun l escaping ->
      String.fold_left begin fun escaping -> function
        | ',' when !parenthesis_depth = 1 ->                           (* skip *)
            false
        | ']' when !parenthesis_depth = 1 ->
            junk := true;
            emit None;                                (* terminate the stream *)
            false
        | '{' | '[' as c when not !in_quotes ->
            incr parenthesis_depth;
            if !parenthesis_depth > 1 then
              Buffer.add_char json c;
            false
        | '}' | ']' as c when not !in_quotes ->
            decr parenthesis_depth;
            Buffer.add_char json c;
            if !parenthesis_depth = 1 then begin
              let j = Buffer.contents json in
              Buffer.clear json;
              if !junk
              then Log.debug "Internal@ warning:@ ignored@ garbage@;%s" j
              else emit @@ Some (Json.read_cbmc_output encoding j)
            end;
            false
        | '\\' as c when !in_quotes ->
            Buffer.add_char json c;
            not escaping
        | '"' as c when not escaping ->
            Buffer.add_char json c;
            in_quotes := not !in_quotes;
            false
        | c ->
            Buffer.add_char json c;
            false
      end escaping l
    end stream false
  end;
  out

let str_of_mode = function
  | Types.OPTIONS.Cover -> "CBMC_COVER_MODE"
  | Assert -> "CBMC_ASSERT_MODE"
  | CLabel -> "CBMC_CLABEL_MODE"

let cbmc_generic_process
    ~encoding
    ~runner_options
    ~timeout
    ~(inputs_json : [>`json] Sc_sys.File.t)
    ~(outputs_json : [>`json] Sc_sys.File.t)
    ~(errors_file : _ Sc_sys.File.t)
    ~(store : Sc_store.t) =
  let resdir = runner_options.runner_resdir
  and mode = runner_options.runner_mode in
  let* inputs_fd =
    Log.debug "input: `%a'" Sc_sys.File.print inputs_json;
    Sc_sys.Lwt_file.descriptor inputs_json [O_RDONLY] 0       (* perm. unused *)
  and* outputs_fd =
    Log.debug "output: `%a'" Sc_sys.File.print outputs_json;
    Sc_sys.Lwt_file.descriptor outputs_json [O_WRONLY; O_CREAT; O_TRUNC] 0o600
  and* errors_fd =
    Log.debug "errors: `%a'" Sc_sys.File.print errors_file;
    Sc_sys.Lwt_file.descriptor errors_file [O_WRONLY; O_CREAT; O_TRUNC] 0o600
  in
  let lines_stream_mbox = Lwt_mvar.create_empty () in
  let* proc =
    Sc_sys.Process.exec
      Sc_sys.Ezcmd.Std.(make "cbmc" |>
                        key "json-interface" |>
                        rawf "-I%a" Sc_sys.File.print resdir |>
                        rawf "-D%s" (str_of_mode mode) |>
                        to_cmd)
      ~stdin:(`FD_move (Lwt_unix.unix_file_descr inputs_fd))
      ~stdout:(`Grab (MBox lines_stream_mbox))
      ~stderr:(`FD_move (Lwt_unix.unix_file_descr errors_fd))
      ~timeout
      ~on_success:(fun () -> Lwt.return_ok ())
      ~on_error:(fun e -> Lwt.return_error e)
  in
  let* cancel_kill =
    Sc_store.on_termination store ~h:(fun _ -> Sc_sys.Process.terminate proc)
  in
  let* output_lines = Lwt_mvar.take lines_stream_mbox in
  Lwt.async begin fun () ->
    let oc = Lwt_io.of_fd outputs_fd ~mode:Output in
    let* () = Lwt_io.write_lines oc (Lwt_stream.clone output_lines) in
    Lwt_io.close oc
  end;
  Lwt.return (decode_cbmc_output_stream encoding output_lines, cancel_kill)

(* From the lannot label identifier, returns the corresponding error label for CBMC *)
let label_of pp s = Format.asprintf "sc_label%a" pp s (* Defined in cbmc_label_driver.h *)

let sc_opt_to_opt
    ?oproperties
    ?(oshow_properties=false)
    ?oerror_label
    ?ocover
    ~ofunction
    ~files
    sc_opt =
  let oproperties =
    match oproperties with
    | None -> None
    | Some {proof_objectives; extra_required_properties; _} ->
        let prop_names =
          (* Concat names of proof objectives and extra props *)
          let po_names = PropertyMap.names proof_objectives in
          List.fold_left
            (fun po_names {pname; _} -> pname :: po_names)
            po_names
            extra_required_properties
        in
        Some prop_names
  in
  {
    oarguments = List.map Sc_sys.File.absname files;
    ofunction;
    ounwind = if sc_opt.OPTIONS.unwind = 0 then None else Some sc_opt.unwind;
    oproperties;
    oshow_properties;
    ocover;
    oerror_label;
    opointer_check = true;
    onondet_static = false;
    omalloc_may_fail = CantFail;
  }

let error_label_of_simple_lbl (l: Sc_C.Cov_label.simple) : string =
  label_of Format.pp_print_int (Sc_C.Cov_label.id l)

let label_of_property p = error_label_of_simple_lbl p

let opt_encoding_and_cmd_options_from_exec_kind
    (type a) (ek : a exec_kind)
    (ofunction : string)
    (files : [`C] Sc_sys.File.t list)
    (sc_opt : OPTIONS.t) : json_options * a Json_encoding.encoding =
  match ek with
  | GetProperties ->
      sc_opt_to_opt
        ~ofunction
        ~oshow_properties:true
        ~files
        sc_opt,
      Json.Output.(cell properties)

  | GetCoverObjectives ->
      sc_opt_to_opt
        ~ofunction
        ~oshow_properties:true
        ~ocover:"cover"
        ~files
        sc_opt,
      Json.Output.(cell properties)

  | GetCLabels lbls ->
      sc_opt_to_opt
        ~ofunction
        ~oshow_properties:true
        ~files
        ~oerror_label:(List.map error_label_of_simple_lbl lbls)
        sc_opt,
      Json.Output.(cell properties)

  | CoverAnalysis (SimpleLabelEnv oproperties) ->
      sc_opt_to_opt
        ~oproperties
        ~ofunction
        ~ocover:"cover"
        ~files
        sc_opt,
      Json.Output.(cell cbmc_cover_output)

  | AssertAnalysis (SimpleLabelEnv oproperties) ->
      sc_opt_to_opt
        ~oproperties
        ~ofunction
        ~files
        sc_opt,
      Json.Output.(cell assert_analysis_result)

  | CLabelAnalysis (SimpleLabelEnv oprops) ->
      let oerror_label =
        PropertyMap.fold
          (fun _ p acc -> label_of_property p :: acc)
          oprops.proof_objectives
          []
      in
      sc_opt_to_opt
        ~ofunction
        ~files
        ~oerror_label
        sc_opt,
      Json.Output.(cell assert_analysis_result)

let write_json ek ~runner_options (options : json_options) : [`json] Sc_sys.File.t Lwt.t =
  let json = Json.options options in
  let file =
    Sc_sys.File.PRETTY.assume_in ~dir:runner_options.runner_inputs
      "%u-%a-options.json" runner_options.runner_iteration pp_execution_kind ek
  in
  let* () = Sc_sys.Lwt_file.write file json in
  Lwt.return file

let out_json ek ~runner_options : [`json] Sc_sys.File.t Lwt.t =
  Lwt.return @@
  Sc_sys.File.PRETTY.assume_in ~dir:runner_options.runner_outputs
    "%u-%a-outputs.json" runner_options.runner_iteration pp_execution_kind ek

let err_file ek ~runner_options : [`stderr] Sc_sys.File.t Lwt.t =
  Lwt.return @@
  Sc_sys.File.PRETTY.assume_in ~dir:runner_options.runner_outputs
    "%u-%a-errors" runner_options.runner_iteration pp_execution_kind ek

let cbmc_start
    (type a)
    (ek : a exec_kind)
    ~(runner_options: runner_options)
    ~(store : Sc_store.t)
    ~(entrypoint : string)
    ~(files : [`C] Sc_sys.File.t list)
    (options : OPTIONS.t) =
  let joptions, encoding =
    opt_encoding_and_cmd_options_from_exec_kind ek entrypoint files options
  in
  let* inputs_json = write_json ek ~runner_options joptions
  and* outputs_json = out_json ek ~runner_options
  and* errors_file = err_file ek ~runner_options in
  cbmc_generic_process ~encoding ~runner_options
    ~store ~timeout:options.timeout ~inputs_json ~outputs_json ~errors_file

let cbmc_get_properties  : property list cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start GetProperties ~store ~runner_options ~entrypoint ~files opt

let cbmc_get_cover_objectives : property list cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start GetCoverObjectives ~store ~runner_options ~entrypoint ~files opt

let cbmc_get_clabels ~lbls : property list cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start (GetCLabels lbls) ~store ~runner_options ~entrypoint ~files opt

let cbmc_cover_analysis ~to_cover : DATA.cbmc_cover_output DATA.cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start (CoverAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt

let cbmc_assert_analysis ~to_cover : cbmc_assert_output DATA.cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start (AssertAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt

let cbmc_clabel_analysis ~to_cover : cbmc_assert_output DATA.cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start (CLabelAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt
