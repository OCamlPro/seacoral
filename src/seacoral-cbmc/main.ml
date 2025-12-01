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

open Sc_core.Types
open Types

open Lwt.Syntax
open Sc_sys.File.Syntax

let toolname = "cbmc"

type 'raw_test working_data =
  {
    harness_file: [`C] Sc_sys.File.t;
    harness_repr: Harness.t;
    project: 'raw_test Sc_project.Types.project;
    validator: 'raw_test Sc_corpus.Validator.ready;
    workspace: Sc_core.Types.workspace [@warning "-unused-field"];
    runner_options: runner_options;
    opt: OPTIONS.t;
  } [@@warning "-unused-field"]                            (* for ocaml < 5.1 *)

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create "Sc_cbmc" ~doc:"Logs of CBMC"))

(* Copies the resource file corresponding to the current cbmc moden then
   generates the harness in the workspace.
   Returns the harness file and its internal representation. *)
let harness_gen ~resdir ~project ~workspace =
  let workdir = workspace.workdir in
  let target = workdir / "harness.c" in
  (* We copy the correct resource file in the work space. This allows
     to only change this file to switch modes. *)
  let cbmc_driver =
    let orig_file = resdir / "cbmc_driver.h" in
    let new_file = workdir / "cbmc_driver.h" in
    Sc_sys.File.link orig_file new_file;
    new_file
  in
  let harness = Harness.generate ~project ~target ~cbmc_driver in
  (target, harness)

let config_section =
  Sc_config.Section.define toolname ~default:Options.default_opt
    ~entries:Options.toml_entries

(** Checks whether cbmc is installed, and cache the result to avoid spawning the
    command several times. *)
let cbmc_is_installed =
  let cache = ref None
  and mutex = Lwt_mutex.create ()
  and cmd = Sc_sys.Ezcmd.Std.(make "cbmc" |> key "version" |> to_cmd) in
  fun () ->
    Lwt_mutex.with_lock mutex begin fun () -> match !cache with
      | None ->
          let* s =
            Sc_sys.Process.exec_status cmd ~stdout:`Dev_null ~stderr:`Dev_null
          in
          cache := Some s;
          Lwt.return s
      | Some s ->
          Lwt.return s
    end

let get_properties ~(mode: Types.OPTIONS.mode) ~lbls =
  match mode with
  | Cover -> Runner.cbmc_get_cover_objectives
  | Assert -> Runner.cbmc_get_properties
  | CLabel -> Runner.cbmc_get_clabels ~lbls

let handle_cover_result
      (type raw_test) ~(wd: raw_test working_data) inputs =
  let params = wd.project.params in
  let module Raw_test = (val params.test_repr) in
  let v = Raw_test.Val.blank params.test_struct in
  Log.debug "@[<2>Blank@ built,@ assigning@ input@ %a@]\
            " Sc_values.pp_literal_binding inputs;
  Raw_test.Val.assign_from_literal params.typdecls v inputs;
  Sc_corpus.Validator.validate_n_share_raw_test wd.validator v
    ~corpus:wd.project.corpus ~toolname ~log_outcome:true

let handle_uncoverable ~wd i =
  Sc_store.share_status ~toolname wd.project.store `Uncov i

let start_cbmc ~wd ~to_cover =
  let files = [wd.harness_file] in
  let store = wd.project.store in
  let runner_options = wd.runner_options in
  let entrypoint = Harness.entrypoint wd.harness_repr in (* Name of the main function in the harness *)
  let SimpleLabelEnv env = to_cover in
  let harness = wd.harness_repr in
  match wd.opt.OPTIONS.mode with
  | Cover ->
     let* stream, cancel_kill =
       Runner.cbmc_cover_analysis
         ~store
         ~runner_options
         ~entrypoint
         ~files
         ~to_cover
         wd.opt
     in
     Lwt.return
       (Results.goal_stream_to_test_cases_stream ~env ~harness ~stream,
        cancel_kill)
  | Assert ->
     let* stream, cancel_kill =
       Runner.cbmc_assert_analysis
         ~store ~runner_options
         ~entrypoint ~files ~to_cover wd.opt
     in
     Lwt.return
       (Results.assert_data_stream_to_test_cases_stream ~env ~harness ~stream,
        cancel_kill)
  | CLabel ->
    let* stream, cancel_kill =
      Runner.cbmc_clabel_analysis
        ~store ~runner_options
        ~entrypoint ~files ~to_cover wd.opt
    in
    Lwt.return
      (Results.assert_data_stream_to_test_cases_stream ~env ~harness ~stream,
       cancel_kill)

let setup ~dry:_ ~(workspace : Sc_core.Types.workspace) ~(opt: OPTIONS.t)
    ~project =
  let* resdir =
    Sc_core.Workspace.install_resources_in ~workspace
      Common.resource_installer
  in
  let harness_file, harness_repr = harness_gen ~resdir ~project ~workspace in
  let runner_iteration = project.config.project_run.run_num in
  let inputs = workspace.workdir / "inputs"
  and outputs = workspace.workdir / "outputs" in
  let* () = Sc_sys.Lwt_file.touch_dir inputs
  and* () = Sc_sys.Lwt_file.touch_dir outputs in
  let* validator = Sc_corpus.Validator.setup project.validator in
  Lwt.return { harness_file; harness_repr;
               project; validator; workspace; opt;
               runner_options = { runner_iteration;
                                  runner_inputs = inputs;
                                  runner_outputs = outputs;
                                  runner_resdir = resdir;
                                  runner_mode = opt.mode } }

let properties_to_verify wd : [`simple] analysis_env option Lwt.t =
  Log.debug "Getting@ properties@ to@ check";
  let Sc_ltest.Types.{simpl; _} = wd.project.labels in
  let entrypoint = Harness.entrypoint wd.harness_repr in (* Name of the main function in the harness *)
  let* data_properties, cancel_kill =
    get_properties
      ~mode:wd.opt.mode
      ~store:wd.project.store
      ~runner_options:wd.runner_options
      ~entrypoint
      ~files:[wd.harness_file]
      ~lbls:simpl
      wd.opt
  in
  (* We could process the payload on the fly instead of putting it in a
     list, but the interesting data only is generated in a single cell at the
     end. *)
  let* data_properties = Lwt_stream.to_list data_properties
  and* () = cancel_kill () in
  match List.flatten @@ Results.only_data data_properties with
  | [] when not (Sc_project.Manager.seeks_oracle_failures wd.project) ->
      Log.warn "Found@ no@ properties@ to@ verify@ on@ the@ C@ file";
      Lwt.return None
  | properties ->
      Log.info "Found@ %i@ properties@ to@ verify@ on@ the@ C@ file"
        (List.length properties);
      let Sc_ltest.Types.{ labelized_file; _ } = wd.project.label_data in
      let* covinfo = Sc_store.covinfo wd.project.store in
      let already_decided =
        Basics.Ints.union covinfo.covered_ids covinfo.uncoverable_ids
      in
      Log.info "There@ were@ already@ %i@ properties@ decided"
        (Basics.Ints.cardinal already_decided);
      Lwt.return @@ Option.some @@
      Runner.uncovered_properties
        ~mode:wd.opt.mode
        ~harness_file:wd.harness_file
        ~labelized_file
        (* ~variables:wd.file.FILE.variable_details *)
        ~cbmc_props:properties
        ~already_decided
        ~labels:simpl
        ~entrypoint:entrypoint

let fold_on_res_stream ~wd rs =
  Lwt_stream.fold_s
    (fun (r : Results.res) acc ->
      let* () =
        match r with
        | `Cov (t, _) -> handle_cover_result ~wd t
        | `Uncov i -> handle_uncoverable ~wd (Ints.singleton i)
        | `NonValidExtra _ -> Lwt.return ()
      in
      Lwt.return (acc @ [r])
    )
    rs
    []

let handle_properties (type raw_test) (wd: raw_test working_data)
    (SimpleLabelEnv env as to_cover) =
  Log.debug "@[<2>Ending@ up@ with@ %i@ properties@ to@ check:@;%a@]"
    (PropertyMap.cardinal env.proof_objectives)
    (PropertyMap.print ?check_equal:None) env.proof_objectives;
  let tic = Unix.gettimeofday () in
  let* rs, cancel_kill = start_cbmc ~wd ~to_cover in
  let* l = fold_on_res_stream ~wd rs in
  let cr = Results.summing_up l in
  let time = Unix.gettimeofday () -. tic in
  let* () = cancel_kill () in
  Log.info "CBMC took %.3fs" time;
  let covered = Results.get_covered cr in
  let uncoverable = Results.get_uncoverable cr in
  let test_inputs = Results.get_tests cr in
  Log.info "Covered: %a" Ints.print covered;
  Log.info "Uncoverable: %a" Ints.print uncoverable;
  (* NB: have we got any guarantee two tests in [test_inputs] are not
     equivalent? *)
  (* TODO: actually measure these stats at the other end of the sharing
     channel (in the corpus). *)
  (* TODO: to simplify that, allow `generate_test_cases` to return a
     [Sc_corpus.Main.raw_input Lwt_stream.t] (optionally). *)
  Sc_project.Manager.report_tool_status wd.project ~toolname (Ok ())
    ~elapsed_time:time ~tests_generated:(List.length test_inputs)

let run wd : unit Lwt.t =
  let* to_cover = properties_to_verify wd in
  match to_cover with
  | None ->
      Lwt.return ()
  | Some to_cover ->
      handle_properties wd to_cover

let read_prev_mode workspace =
  Lwt.catch begin fun () ->
    let* mode_str = Sc_sys.Lwt_file.read (workspace.workdir / ".prev-mode") in
    Lwt.return_some (Options.mode_of_string mode_str)
  end begin fun _ ->
    Lwt.return_none
  end

let write_mode workspace mode =
  Lwt.catch begin fun () ->
    Sc_sys.Lwt_file.write (workspace.workdir / ".prev-mode")
      (Options.string_of_mode mode)
  end begin fun _ ->
    Lwt.return ()
  end

module M = struct
  type 'raw_test ready = 'raw_test working_data

  let setup workspace ~optional project : _ ready Lwt.t =
    (* TODO: do not inhibit if the mode changes! *)
    let opt = Sc_config.Section.get config_section in
    let* prev_mode =
      if not optional
      then Lwt.return_none
      else read_prev_mode workspace
    in
    match prev_mode with
    | Some mode when mode = opt.mode ->
        Log.info "Preprocessing for `%a' (inhibited)\
                 " Sc_project.Printer.pp_entrypoint_name project;
        setup ~dry:true ~workspace ~opt ~project
    | Some _ | None ->
        Log.info "Preprocessing for `%a'\
                 " Sc_project.Printer.pp_entrypoint_name project;
        let* res = setup ~dry:false ~workspace ~opt ~project in
        let* () = write_mode workspace opt.mode in
        Lwt.return res

  let run = run
end

let availability_check () =
  cbmc_is_installed ()

let load () =
  Sc_lib.Tool.register_module For_simple_labels (module M)
    ~name:"cbmc" ~availability_check
    ~config_section:(Some config_section)
    ~kind:`Static
