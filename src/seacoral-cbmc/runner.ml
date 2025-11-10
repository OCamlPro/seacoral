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
  'a Lwt_stream.t

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

let id_from_property_name prop =
  match String.split_on_char '.' prop.pname with
  | [fname; kind; id] -> fname, kind, int_of_string id
  | _ -> assert false

let empty_env =
  { extra_required_properties = [];
    proof_objectives = PropertyMap.empty;
    already_proven = PropertyMap.empty }

let property_kind_matches_mode ~mode ~kind =
  match mode with
  | OPTIONS.Cover -> kind = "coverage"
  | Assert -> kind = "assertion"
  | CLabel -> kind = "error_label"

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
      (* Log.debug "Property %a" Printer.pp_property prop; *)
      let fname, kind, lbl_id = id_from_property_name prop in
      Log.debug "Property@ %s.%i@ of@ kind@ %s" fname lbl_id kind; 
      (* Checking if the property belongs to the main file and the main function. *)
      if not (property_kind_matches_mode ~mode ~kind) then
        { env with
          extra_required_properties = prop :: env.extra_required_properties }
      else if property_belongs_to_file ~file:harness_file prop && fname = entrypoint then
        (* Probably unsafe *)
        match Basics.IntMap.find_opt lbl_id lbl_map with
        | None ->                                               (* not a label *)
            { env with
              extra_required_properties = prop :: env.extra_required_properties }
        | Some lbl when
               Sc_C.Cov_label.is_unknown lbl &&
               not (Basics.Ints.mem lbl_id already_decided) ->(* unknown status *)
            { env with
              proof_objectives = PropertyMap.add prop lbl env.proof_objectives }
        | Some lbl ->                                          (* known status *)
            { env with
              already_proven = PropertyMap.add prop lbl env.already_proven }
      else if property_belongs_to_file ~file:labelized_file prop then
        { env with
          extra_required_properties = prop :: env.extra_required_properties }
      else begin
        (* We discard properties that are not in the entrypoint function. *)
        Log.debug "Discarding@ property@ %s:@;source@ files@ differ@ \
                   (@[%s@ <>@ %s)@]"
          prop.pname
          prop.psource_location.slfile
          (Sc_sys.File.name labelized_file);
        env
      end
    end empty_env cbmc_props
  in
  SimpleLabelEnv env

let treat_cbmc_output_stream
      ~stream
      ~handle_line
      ~handle_json_object =
  let json = Buffer.create 42 in
  let fmt = Format.formatter_of_buffer json in
  let parenthesis_depth = ref 0 in
  Lwt.catch (fun () ->
      Lwt_stream.iter_s
        (fun l ->
          let* () = handle_line l in
          let l_no_space = String.trim l in
          let () =
            if l_no_space = "" then ()
            else
              match l_no_space.[0] with
              | '[' when !parenthesis_depth = 0 -> parenthesis_depth := 1
              | _ ->
                 String.iter
                   (fun c ->
                     match c with
                     | '{' ->
                        incr parenthesis_depth;
                        Format.pp_print_char fmt c;
                     | '}' ->
                        decr parenthesis_depth;
                        Format.pp_print_char fmt c;
                        if !parenthesis_depth = 1 then
                          begin
                            Format.pp_print_flush fmt ();
                            handle_json_object (Buffer.contents json);
                            Buffer.clear json
                          end
                     | ',' when !parenthesis_depth = 1 -> ()
                     | _ ->
                        Format.pp_print_char fmt c;
                   ) l;
                 Format.pp_print_char fmt '\n'
          in
          Lwt.return ()
        )
        stream
    )
    (function
     | Lwt_io.Channel_closed _ -> Lwt.return ()
     | exn -> Lwt.reraise exn)

let cbmc_generic_process
    ~push_in_resstream
    ~resdir
    ~timeout
    ~(inputs_json : [>`json] Sc_sys.File.t)
    ~(outputs_json : [>`json] Sc_sys.File.t)
    ~(errors_file : _ Sc_sys.File.t)
    ~(store : Sc_store.t) : _ result Lwt.t =
  let* inputs_fd =
    Log.debug "input: `%a'" Sc_sys.File.print inputs_json;
    Sc_sys.Lwt_file.descriptor inputs_json [O_RDONLY] 0       (* perm. unused *)
  and* outputs_fd =
    Log.debug "output: `%a'" Sc_sys.File.print inputs_json;
    Sc_sys.Lwt_file.descriptor outputs_json [O_WRONLY; O_CREAT; O_TRUNC] 0o600
  and* errors_fd =
    Log.debug "errors: `%a'" Sc_sys.File.print errors_file;
    Sc_sys.Lwt_file.descriptor errors_file [O_WRONLY; O_CREAT; O_TRUNC] 0o600
  in
  let close_stream () = push_in_resstream None in
  let* proc =
    Sc_sys.Process.exec
      Sc_sys.Ezcmd.Std.(make "cbmc" |>
                        key "json-interface" |>
                        rawf "-I%a" Sc_sys.File.print resdir |>
                        to_cmd)
      ~stdin:(`FD_move (Lwt_unix.unix_file_descr inputs_fd))
      ~stdout:`Keep
      ~stderr:(`FD_move (Lwt_unix.unix_file_descr errors_fd))
      ~timeout
      ~on_success:(fun () -> close_stream (); Lwt.return_ok ())
      ~on_error:(fun e -> close_stream (); Lwt.return_error e)
  in
  let _ : unit Lwt.t =
    treat_cbmc_output_stream
      ~stream:(Sc_sys.Process.stdout_lines proc)
      ~handle_line:(fun l ->
        let b = Bytes.of_string (l ^ "\n") in
        let len = Bytes.length b in
        let () =
          Lwt.async (fun () ->
              let* _len_w = Lwt_unix.write outputs_fd b 0 len in
              Lwt.return ()
            )
        in
        Lwt.return ()
      )
      ~handle_json_object:(fun json -> push_in_resstream (Some json))
  in
  let* _cancel_kill =
    Sc_store.on_termination store ~h:(fun _ -> Sc_sys.Process.terminate proc)
  in
  Sc_sys.Process.join proc

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
    ~(files : [`C] Sc_sys.File.t list) (options : OPTIONS.t) =
  let joptions, encoding =
    opt_encoding_and_cmd_options_from_exec_kind ek entrypoint files options
  in
  let resstream, push_in_resstream = Lwt_stream.create () in
  let _status =
    let* inputs_json = write_json ek ~runner_options joptions
    and* outputs_json = out_json ek ~runner_options
    and* errors_file = err_file ek ~runner_options in
    cbmc_generic_process ~push_in_resstream ~resdir:runner_options.runner_resdir
      ~store ~timeout:options.timeout ~inputs_json ~outputs_json ~errors_file
  in
  Lwt_stream.map (Json.read_cbmc_output encoding) resstream

let cbmc_get_properties : property list cell cbmc_run =
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

let cbmc_assert_analysis  ~to_cover : cbmc_assert_output DATA.cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start (AssertAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt

let cbmc_clabel_analysis ~to_cover : cbmc_assert_output DATA.cell cbmc_run =
  fun ~store ~runner_options ~entrypoint ~files opt ->
  cbmc_start (CLabelAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt
