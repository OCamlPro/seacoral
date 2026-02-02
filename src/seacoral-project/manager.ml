(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_core.Types
open Sc_store.Types
open Sc_ltest.Types
open Sc_corpus.Types
open Sc_sys.File.TYPES
open Types

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

(* Note: this module is starting to look like it should be split into a main
   manager and `Sc_project.Preproc`. Preproc is mostly providing project
   `initialization` (codebase retrieval, syntax check, & labeling). *)

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create"Sc_project" ~doc:"Logs of Seacoral's general project \
                                          management"))

let install_include_dir_in: workspace:_ -> dir Lwt.t =
  Sc_core.Workspace.install_resources_in @@
  Sc_core.Resource.register_crunched "include" (module Resources)

(* --- *)

let run_info project =
  project.config.project_run

let problem project =
  project.config.project_problem

let has_oracle project =
  project.params.oracle_func <> None

let seeks_oracle_failures project =
  project.params.seek_oracle_failures && has_oracle project

let entrypoint_name project =
  project.extra.given_entrypoint_name

(* --- *)

let log_unexpected_errors: (unit -> 'a Lwt.t) -> 'a Lwt.t = fun f ->
  Log.LWT.err_if_rejected f ~silence:begin function
    | SETUP_ERROR _
    | LABELING_ERROR _
    | ELABORATION_ERROR _ -> true
    | _ -> false
  end

(* --- *)

let setup_error err =
  raise @@ SETUP_ERROR err

let check_external_tools_availability () =
  Lwt.catch begin fun () ->
    Sc_ltest.Lannot.check_availability ()
  end begin function
    | Sc_ltest.Types.ERROR e -> setup_error (Ltest e)
    | e -> Lwt.reraise e
  end

(* --- *)

let make_ltest_module ~workspace config : (module Sc_ltest.Main.S) =
  (module Sc_ltest.Main.Make (struct
       let workspace = workspace
       let config = config.project_problem
     end))

(* --- *)

type preprocessed_project =
  {
    workspace: workspace;
    config: project_config;
    codebase: [`C] file;
    label_data: label_data;
    given_entrypoint_name: string;
  }

let labeling_error err =
  raise @@ LABELING_ERROR err

let failed_labeling ~workdir_status = function
  | LABELING_ERROR _ as e -> Lwt.reraise e
  | error -> labeling_error @@ Failed_labeling { workdir_status; error }

let elaboration_error err =
  raise @@ ELABORATION_ERROR err

let failed_elaboration = function
  | ELABORATION_ERROR _ as e -> Lwt.reraise e
  | error -> elaboration_error @@ Failed_elaboration error

let make_labeling_module config =
  make_ltest_module config
    ~workspace:(Sc_core.Workspace.mksub config.project_workspace "labeling")

let do_labeling ~incdir ~workspace config =
  Log.info "Gathering the codebase...";
  let labelingdir = Sc_sys.File.mkdir_in ~dir:workspace.workdir "labeling" in
  let given_entrypoint_name = config.project_problem.entrypoint in
  let config = Preproc.patch_entrypoint_name config in
  let* codebase = Preproc.gather_codebase_in ~dir:labelingdir config in

  let* () =
    if config.project_check_syntax
    then (Log.info "Syntax-checking the codebase...";
          Preproc.do_syntax_check ~incdir ~config codebase)
    else Lwt.return ()
  in

  Log.info "Labeling the codebase...";
  let module Ltest = (val make_labeling_module config) in
  let* label_data =
    Ltest.Lannot.create_annotated_file codebase ~include_dirs:[incdir]
  in
  Log.info "Labeling done";
  Lwt.return { workspace; config; codebase; label_data; given_entrypoint_name }

let skip_labeling ~workspace config =
  Log.info "Skipping labeling phase";
  let labelingdir
    = Sc_sys.File.existing_dir_in ~dir:workspace.workdir "labeling" in
  let given_entrypoint_name = config.project_problem.entrypoint in
  let config = Preproc.patch_entrypoint_name config in
  let codebase = Preproc.codebase_file_in ~dir:labelingdir config in

  let module Ltest = (val make_labeling_module config) in
  let label_data = Ltest.Lannot.assume_annotated_file codebase in
  Lwt.return { workspace; config; codebase; label_data; given_entrypoint_name }

let setup_project_params ~test_repr { config; label_data; _ } =
  Lwt.catch begin fun () ->
    Preproc.setup_for ~c_file:label_data.labelized_file
      ~config ~test_repr
  end begin function
    | Sc_C.Types.Unknown_function fun_name ->
        let c_file =
          match config.project_problem.input_files with
          | [f] -> Some f
          | _ -> None
        in
        elaboration_error @@ Unknown_function { fun_name; c_file }
    | e ->
        Lwt.reraise e
  end

let update_label_status ~covinfo label =
  let id = Sc_C.Cov_label.id label in
  match Sc_C.Cov_label.status label with
  | Uncoverable when Basics.Ints.mem id covinfo.covered_ids ->
      Log.err
        "Store says label %u is coverable, while label database says it is \
         uncoverable." id;
      label
  | Covered _ when Basics.Ints.mem id covinfo.uncoverable_ids ->
      Log.err
        "Store says label %u is uncoverable, while label database says it is \
         covered." id;
      label
  | _ ->
      if Basics.Ints.mem id covinfo.covered_ids then
        Sc_C.Cov_label.set_status (Covered []) label
      else if Basics.Ints.mem id covinfo.uncoverable_ids then
        Sc_C.Cov_label.set_status Uncoverable label
      else label

let update_labels_status ~covinfo { simpl; hyper; _ } =
  let simpl = List.map (update_label_status ~covinfo) simpl in
  let hyper = List.map (update_label_status ~covinfo) hyper in
  let any =
    List.map Sc_C.Cov_label.as_any simpl @
    List.map Sc_C.Cov_label.as_any hyper
  in
  { simpl; hyper; any }

let write_headers_in ~workspace (params: _ project_params) =
  let emit_fixture_declarations ppf =
    match params.init_func, params.oracle_func with
    | None, None ->
        ()
    | Some f, None | None, Some f ->
        Fmt.pf ppf
          "@\n/* Additional fixture */\
           @\n%a\
           @\n"
          (Sc_C.Printer.pp_fundecl ~enable_static_attr:false) f
    | Some f1, Some f2 ->
        Fmt.pf ppf
          "@\n/* Additional fixtures */\
           @\n%a\
           @\n%a\
           @\n"
          (Sc_C.Printer.pp_fundecl ~enable_static_attr:false) f1
          (Sc_C.Printer.pp_fundecl ~enable_static_attr:false) f2
  in
  let func = params.func_repr in
  let types_h = workspace.workdir / "types.h"
  and entry_h = workspace.workdir / "entry.h" in
  let* () =
    Log.debug "Generating `%a'" Sc_sys.File.print types_h;
    let>*% ppf = types_h in
    Lwt_fmt.fprintf ppf
      "/* Auto-generated file */\
       @\n#ifndef __SC_TYPES_H__\
       @\n#define __SC_TYPES_H__\
       @\n\
       @\n/* Type declarations, if any */\
       @\n%a\
       @\n\
       @\n#endif /* __SC_TYPES_H__ */\
       @."
      Sc_C.Printer.pp_typ_defs params.cil
  and* () =
    Log.debug "Generating `%a'" Sc_sys.File.print entry_h;
    let>*% ppf = entry_h in
    Lwt_fmt.fprintf ppf
      "/* Auto-generated file */\
       @\n#ifndef __SC_ENTRY_H__\
       @\n#define __SC_ENTRY_H__\
       @\n\
       @\n/* Global variables, if any */\
       @\n%a\
       @\n\
       @\n/* Tested function */\
       @\n%a\
       @\n%t\
       @\n#endif /* __SC_ENTRY_H__ */\
       @."
      Sc_C.Printer.pp_func_env func.func_env
      (Sc_C.Printer.pp_fundecl ~enable_static_attr:false) func
      emit_fixture_declarations
  in
  Lwt.return (types_h, entry_h)

let elaborate ~test_repr ({ workspace; codebase; config; label_data;
                            given_entrypoint_name; _ } as preprocessed) =
  let outdir = workspace.workdir / "tests" in
  let covdir = outdir / "cov"
  and rtedir = outdir / "rte" in
  let statsdir = workspace.workdir / "stats" in
  let* () = Sc_sys.Lwt_file.touch_dir covdir
  and* () = Sc_sys.Lwt_file.touch_dir rtedir
  and* () = Sc_sys.Lwt_file.touch_dir statsdir
  and* faildir =
    if not config.project_problem.seek_oracle_failures then Lwt.return_none else
      let faildir = outdir / "fail" in
      let* () = Sc_sys.Lwt_file.touch_dir faildir in
      Lwt.return_some faildir
  in

  let* params = setup_project_params ~test_repr preprocessed in
  let labels = Sc_ltest.Label_database.get_lbls label_data.label_file in
  let* corpus =
    let* workspace = Sc_core.Workspace.LWT.mksub workspace "corpus" in
    Sc_corpus.make ~workspace
      { test_repr;
        test_struct = params.test_struct;
        run_num = config.project_run.run_num }
  and* store =
    let* workspace = Sc_core.Workspace.LWT.mksub workspace "store" in
    Sc_store.make ~workspace
      { num_labels = List.length labels.any;
        inhibit_auto_termination = params.oracle_func <> None }
  and* decoder =
    let* workspace = Sc_core.Workspace.LWT.mksub workspace "decoder" in
    Sc_corpus.Decoder.make ~workspace { test_struct = params.test_struct;
                                        cil = params.cil;
                                        test_repr = params.test_repr }
  and* types_header, func_header =
    let* workspace = Sc_core.Workspace.LWT.mksub workspace "headers" in
    write_headers_in ~workspace params
  in
  let* validator =
    let* workspace = Sc_core.Workspace.LWT.mksub workspace "validator" in
    Sc_corpus.Validator.make ~workspace decoder store
      { cil = params.cil;
        func_repr = params.func_repr;
        func_header;
        test_repr = params.test_repr;
        test_struct = params.test_struct;
        test_timeout = Some config.project_test_timeout;
        max_concurrent_validations = config.project_max_validation_concurrency;
        validator_verbosity =
          if config.project_verbose_validation then 1 else 0;
        init_func = params.init_func;
        oracle_func =
          if params.seek_oracle_failures then params.oracle_func else None;
        labelized_file = label_data.labelized_file }
  in
  let* covinfo = Sc_store.covinfo store in
  let labels = update_labels_status ~covinfo labels in

  let stats =
    Statistics.make ~dir:statsdir ~run_num:config.project_run.run_num
  in
  Lwt.return { config; params; workspace; codebase; label_data; labels;
               types_header; func_header;
               store; corpus; decoder; validator;
               outdir; covdir; rtedir; faildir; stats;
               extra = { given_entrypoint_name } }

(** Initializes a project.

    An input project is either a raw project to prepare for other tools, or a
    project already prepared that a tool may want to customize. *)
let initialize
    ~initialization_options
    ~test_repr
    ~config:({ project_workspace = workspace; _ } as config)
  =
  let* () = check_external_tools_availability () in
  let* incdir = install_include_dir_in ~workspace in
  Log.debug "Initializing";
  log_unexpected_errors begin fun () ->
    Sc_sys.Lwt_lazy.persist_in ~dir:workspace.workdir
      ~force:initialization_options.force_preprocess
      ~donefile:".labeling-done"
      begin fun () ->
        Lwt.catch
          (fun () -> do_labeling ~workspace ~incdir config)
          (failed_labeling ~workdir_status:`Created)
      end
      begin fun () ->
        Lwt.catch
          (fun () -> skip_labeling ~workspace config)
          (failed_labeling ~workdir_status:`Reused)
      end
    >>= fun preprocessed ->
    Lwt.catch
      (fun () -> elaborate ~test_repr preprocessed)
      (failed_elaboration)
  end

(* --- *)

let info project =
  let* cov = Sc_store.covinfo project.store in
  Lwt.return (cov, Sc_corpus.info project.corpus)

(* --- *)

let report_tool_status { stats; _ } ~toolname ~elapsed_time
    ~tests_generated status =
  (* TODO: check against tests_generated from the store! *)
  Lwt.return @@
  Statistics.report ~stats ~tool:toolname
    { time = elapsed_time; status; tests_generated }

let save_tool_statistics { stats; _ } =
  Statistics.save stats

let print_tool_statistics ppf { stats; _ } =
  Statistics.print ppf stats
