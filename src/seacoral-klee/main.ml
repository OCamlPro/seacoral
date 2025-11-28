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
open Sc_project.Types
open Sc_sys.File.TYPES
open Config.TYPES
open Harness.TYPES

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_lazy.Syntax

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create "Sc_klee" ~doc:"Logs of Klee"))

let harness_options =
  {
    symbolization_scheme = `independent_fields (* `full_struct *);
  }

type 'raw_test working_data =
  {
    options: Config.TYPES.options;
    workspace: Sc_core.Types.workspace;
    resdir: dir [@warning "-unused-field"];
    project: 'raw_test Sc_project.Types.project;
    validator: 'raw_test Sc_corpus.Validator.ready;
    harness_bc: [`BC] file;
    harness_entrypoint: string;
    runtime_params: Harness.runtime_params;
    ktest_io: (module Ktest.IO with type raw_test = 'raw_test);
  } [@@warning "-unused-field"]                            (* for ocaml < 5.1 *)

type klee_dirs =
  {
    seedsdir: dir option;        (** Directory where klee may find some seeds *)
    workdir: dir;                (** Output directory for klee *)
  }

(* --- *)

let install_resources_in: workspace:_ -> dir Lwt.t =
  Sc_core.Workspace.install_resources_in @@
  Sc_core.Resource.register_crunched "klee" (module Resources)

(* When klee generates a new ".ktest" file, it additionally generates an
   additional ".err" file.
   A ".assert.err" file means that a klee assertion has been raised, which are
   used when a label is triggered -> this input covers a label.
   A ".user/ptr/div/overshift.err" means klee found an RTE.

   It may be possible a ktest file is created while its err counterpart does
   not exist yet (and vice-versa). *)

let cov_suffix = ".assert.err"
let rte_suffixes = [".user.err"; ".ptr.err"; ".div.err"; ".overshift.err"]

(* (\* Returns the input effect of the ktest input. *)
(*    Returns None if the input effect is unknown. *\) *)
(* let effect_from_ktest f : Sc_corpus.Types.test_outcome option = *)
(*   let base = Sc_sys.File.chop_suffix f ".ktest" in *)
(*   if Sys.file_exists (base ^ cov_suffix) *)
(*   then Some Covering_label *)
(*   else if List.exists (fun s -> Sys.file_exists (base ^ s)) rte_suffixes *)
(*   then Some Triggering_RTE *)
(*   else None *)

(* (\* Same as `effect_from_ktest`, but from the err couterpart. *\) *)
(* let effect_from_err f : Sc_corpus.Types.test_outcome option = *)
(*   if Sc_sys.File.check_suffix f cov_suffix *)
(*   then Some Covering_label *)
(*   else if List.exists (Sc_sys.File.check_suffix f) rte_suffixes *)
(*   then Some Triggering_RTE *)
(*   else None *)

let pp_include ppf filename =
  Fmt.pf ppf "#include %S@\n" filename

let build_harness (workspace: workspace) project =
  Log.info "Generating@ harness@ for@ `%a'\
           " Sc_project.Printer.pp_entrypoint_name project;
  let harness = workspace.workdir / "harness.c" in
  let support_data = Harness.support_data harness_options project.params in
  let harness_entrypoint = Harness.entrypoint support_data in
  let runtime_params = Harness.runtime_params support_data in
  let { pp_preamble; pp_body } = Harness.printer support_data in
  let>% ppf = harness in
  Log.debug "Writing@ harness@ file@ `%a'" Sc_sys.File.print harness;
  pp_preamble ppf;
  pp_include ppf "driver.c";
  pp_include ppf "tested.c";
  pp_body ppf;
  Lwt.return (harness, harness_entrypoint, runtime_params)

let reuse_harness project =
  Log.info "Reusing@ harness@ for@ `%a'\
           " Sc_project.Printer.pp_entrypoint_name project;
  let support_data = Harness.support_data harness_options project.params in
  let harness_entrypoint = Harness.entrypoint support_data in
  let runtime_params = Harness.runtime_params support_data in
  Lwt.return (harness_entrypoint, runtime_params)

let label_handling_cppflags = function
  | Ignore ->
      ["-DKLEE_IGNORE_LABELS"]
  | Naive ->
      ["-DKLEE_IN_NAIVE_MODE"]
  | Optimized { benign_test_policy = Reject | Avoid } ->
      ["-DKLEE_AVOID_BENIGN_TESTS"]
  | Optimized _ ->
      []

let compile_to_bitcode ~resdir ~incdir options c_file =
  Log.debug "Compiling@ `%a'@ to@ LLVM@ bitcode" Sc_sys.File.print c_file;
  Sc_C.Cmd.clang_llvm c_file
    ~cppflags:([ "-g";
                 "-I"; Sc_sys.File.name resdir;
                 "-I"; Sc_sys.File.name incdir ]
               @ label_handling_cppflags options.label_handling)

(* --- *)

let preprocess (type raw_test) workspace (project: raw_test project)
  : raw_test working_data Lwt.t =
  let options = Sc_config.Section.get Config.section in
  let* resdir = install_resources_in ~workspace in
  let* incdir = Sc_project.Manager.install_include_dir_in ~workspace in
  let driver_c = workspace.workdir / "driver.c"
  and tested_c = workspace.workdir / "tested.c" in
  let* () = Sc_sys.Lwt_file.link (resdir / "klee_driver.c") driver_c
  and* () = Sc_sys.Lwt_file.link project.label_data.labelized_file tested_c
  and* harness_c, harness_entrypoint, runtime_params =
    build_harness workspace project
  in
  let* harness_bc = compile_to_bitcode ~resdir ~incdir options harness_c in
  let* validator = Sc_corpus.Validator.setup project.validator in
  let module Test_repr = (val project.params.test_repr) in
  Lwt.return { options; workspace; resdir; project; validator;
               harness_bc; harness_entrypoint; runtime_params;
               ktest_io = (module Ktest.Make_io (Test_repr)); }

let no_preprocess (type raw_test) workspace (project: raw_test project)
  : raw_test working_data Lwt.t =
  let options = Sc_config.Section.get Config.section in
  let* resdir = install_resources_in ~workspace in
  let* harness_entrypoint, runtime_params = reuse_harness project in
  let workdir = workspace.workdir in
  let harness_bc = Sc_sys.File.existing_in ~dir:workdir "harness.bc" in
  let* validator = Sc_corpus.Validator.setup project.validator in
  let module Test_repr = (val project.params.test_repr) in
  Lwt.return { options; workspace; resdir; project; validator;
               harness_bc; harness_entrypoint; runtime_params;
               ktest_io = (module Ktest.Make_io (Test_repr)); }

let setup ws ~optional p =
  (if optional then no_preprocess else preprocess) ws p

(* --- *)

(* (\* Function called upon termination of the main process, to share inputs that *)
(*    may have been missed by the directory sharing mechanim (note this is quite *)
(*    unlikely).  Those tests are NOT replayed.  *\) *)
(* let ktest_results wd { workdir; _ } = *)
(*   Sc_sys.Lwt_file.files_of_dir workdir |> *)
(*   Lwt_stream.filter (fun f -> Sc_sys.File.check_suffix f ".ktest") |> *)
(*   Lwt_stream.filter_map begin fun ktest -> *)
(*     let base = Filename.chop_suffix (Sc_sys.File.name ktest) ".ktest" in *)
(*     let regular f = Sys.file_exists f && not (Sys.is_directory f) in *)
(*     match wd.options.label_handling with *)
(*     | Ignore | Naive *)
(*       when not wd.options.keep_error_cases && *)
(*            (List.exists (fun s -> Sys.file_exists (base ^ s)) *)
(*               (cov_suffix :: rte_suffixes)) -> *)
(*         None *)
(*     | Optimized { benign_test_policy = Reject } *)
(*       when not (regular (base ^ cov_suffix)) -> *)
(*         None *)
(*     | _ -> *)
(*         Option.map (fun e -> e, ktest) (effect_from_ktest ktest) (\* ignore if no effect *\) *)
(*   end *)

(* Log module for reporting klee's outputs *)
module Log_lwt_klee = (val Ez_logs.subproc "klee")

(* TODO: make klee options customizable *)
let start_klee wd { workdir; seedsdir; _ }
  : (unit Sc_sys.Process.t * (unit -> unit Lwt.t)) Lwt.t =
  let timeout = wd.options.timeout in
  Log.info "Staring@ klee@ on@ `%a'@ with@ %t" Sc_sys.File.print wd.harness_bc
    (if timeout = 0.
     then fun ppf -> Fmt.(styled `Yellow @@ any "no@ timeout") ppf ()
     else fun ppf -> Fmt.(fmt "a@ timeout@ of@ %.2fs" ppf timeout));
  let* seedsdir =
    (* If the seeds dir is empty, klee complains. *)
    match seedsdir with
    | None ->
        Lwt.return None
    | Some dir ->
        let* is_empty = Sc_sys.Lwt_file.dir_is_empty dir in
        if is_empty
        then Lwt.return None
        else Lwt.return seedsdir
  in
  let seeding_options =
    match seedsdir with
    | Some dir ->
        [ Fmt.str "--seed-dir=%s" (Sc_sys.File.name dir);
          "--seed-time=20s";                           (* TODO: new parameter *)
          "--allow-seed-extension"; (* when seeding, handle unspecified inputs as symbolic *)
          "--always-output-seeds=false" ] (* do not put seed inputs in output dir *)
    | None ->
        []
  in
  let* { env; _ } = Sc_store.for_compiled_subprocess wd.project.store in
  let* process =
    Sc_sys.Process.exec
      (Array.of_list @@
       [ "klee";
         Fmt.str "--entry-point=%s" wd.harness_entrypoint;
         Fmt.str "--libc=%s" wd.options.libc;
         (* "--search=nurs:covnew"; *)
         (* "--external-calls=all"; *)
         (* "--use-merge"; *)
         (* "--solver-backend=stp"; *)
         "--size-model=range";
         "--capacity"; Int.to_string wd.runtime_params.symsize_capacity;
         "--return-null-on-zero-malloc=false";
         Fmt.str "--output-dir=%a" Sc_sys.File.print_absname workdir;
         "--only-output-states-covering-new";
         "--dump-states-on-halt=false";     (* do not emit any .ktest on halt *)
         "--silent-klee-assume";
         (* "--watchdog";        (\* use a watchdog process to enforce max-time *\) *)
         Fmt.str "--max-time=%.0f" wd.options.timeout ]
       @ List.map (Format.asprintf "--search=%s") wd.options.search
       @ seeding_options
       @ [ Sc_sys.File.name wd.harness_bc ])
      ~env
      (* Note: we purposefully increase the timout here so we have a chance to
         get some info out of klee when its own watchdog fires: *)
      ~timeout:(wd.options.timeout +. 10.)
      ~stdout:(`Log Log_lwt_klee.LWT.debug)
      ~stderr:(`Log Log_lwt_klee.LWT.debug)
      ~on_success:Lwt.return
      ~on_error:begin function
        | Unix.WSIGNALED -7 ->
            Log.LWT.info "Klee timed out"
        | s ->
            Log.LWT.err "Error while working with klee: %a\
                        " Sc_sys.Process.pp_unix_status s
      end
  in
  let* cancel_kill =
    Sc_store.on_termination wd.project.store
      ~h:(fun _ -> Sc_sys.Process.kill ~delay:1. Sys.sigint process)
  in
  Lwt.return (process, cancel_kill)

(* --- *)

let read_ktest (type r) ({ ktest_io = (module Ktest_io);
                           _ } as wd: r working_data) f =
  Ktest_io.read f wd.project.params.test_struct harness_options

let write_ktest  (type r) ({ ktest_io = (module Ktest_io);
                             _ } as wd: r working_data) f v =
  Ktest_io.write f wd.project.params.test_struct harness_options v

let import_seeds wd indir =
  (* First, remove every exising file in libfuzzer corpus (this could be
     optimized a bit, but is a simple approach to avoid having duplicates
     (re-)imported form the runtime database). *)
  Log.debug "Importing seeds, if any";
  let* () = Sc_sys.Lwt_file.unlink_files_of_dir indir in
  let* seeds =
    Sc_corpus.Sharing.import_tests wd.project.corpus indir
      ~filter:(fun m -> m.outcome = Covering_label)
      ~import_suff:".ktest"
      ~write_test:(`Func (write_ktest wd))
  in
  if Digests.is_empty seeds
  then Lwt.return None
  else begin
    Log.info "Copied@ %i@ seeds@ into@ `%a'\
             " (Digests.cardinal seeds) Sc_sys.File.print indir;
    Lwt.return (Some indir)
  end

let start_ktests_monitoring wd ~replay_test klee_dirs =
  Log.debug "Starting@ to@ monitor@ directory@ `%a'\
            " Sc_sys.File.print klee_dirs.workdir;
  (* The .ktest file is generated before or after their .err counterpart.  When
     we filter the monitored file, we have to keep track of both and check that
     both are alive simultaneously. *)
  Sc_sys.Lwt_watch.ASYNC.monitor_dir klee_dirs.workdir ~on_close:begin fun f ->
    if not (Sc_sys.File.check_suffix f ".ktest")
    then Lwt.return ()
    else begin
      let base = Sc_sys.File.chop_suffix f ".ktest" in
      let* ok_keep = match wd.options.label_handling with
        | Ignore | Naive when not wd.options.keep_error_cases ->
            Lwt.return (wd.options.replay <> Delayed) &&*
            lazy begin
              Lwt_list.exists_p (fun s -> Lwt_unix.file_exists (base ^ s))
                (cov_suffix :: rte_suffixes) >|=
              (not)
            end
        | Optimized { benign_test_policy = Reject } ->
            (* FIXME: .err may not exist yet *)
            Sc_sys.Lwt_file.exists @@ Sc_sys.File.assume (base ^ cov_suffix)
        | Ignore | Naive
        | Optimized { benign_test_policy = Keep | Avoid } ->
            (* keep tests anyways *)
            Lwt.return true
      in
      if ok_keep
      then replay_test f
      else Lwt.return ()
    end
  end

let replay_ktest wd ktest =
  Log.LWT.debug "Considering@ `%a'" Sc_sys.File.print ktest >>= fun () ->
  read_ktest wd ktest >>=
  Sc_corpus.Validator.validate_n_share_raw_test wd.validator
    ~corpus:wd.project.corpus ~toolname:"klee"

let replay_all wd ktest_files =
  let max_concurrency =
    if wd.options.replay_max_concurrency = 0
    then max_int
    else max 1 wd.options.replay_max_concurrency
  in
  Lwt_stream.iter_n ~max_concurrency
    (fun (ktest, resbox) -> Lwt_mvar.put resbox =<< replay_ktest wd ktest)
    ktest_files

(** [start_replayers wd] starts a concurrent component and returns associated
    functions [on_test] and [stop]: [on_test] concurrently launches replayers
    for each new [ktest] file received, whereas [stop] waits for previously
    launched replayers to terminate, and then returns.

    Disables replayers whenever [params.options.replay_max_concurrency < 0 ||
    params.options.replay <> Forced] and [params.options.label_handling <> Naive].

    Note [label_handling = Ignore] takes precedence over [replay = Inhibit], so
    we can account for label-based coverage via calls to `[__sc_covered]` during
    replays.  Replay while [label_handling = Naive] is possible but useless. *)
let start_replayers wd =
  let ok = match wd.options.label_handling, wd.options.replay with
    |(Ignore | Naive), _ | _, (Delayed | Force) -> true
    | _, Inhibit -> false
    | _ -> wd.options.replay_max_concurrency >= 0
  in
  if not ok
  then Lwt.return ((fun _ktest -> Lwt.return ()), Lwt.return)
  else begin
    let ktests_cache = Hashtbl.create 10
    and ktests_cache_lock = Lwt_mutex.create ()
    and ktests_mbox = Lwt_mvar.create_empty ()
    and replayers_done = Lwt_condition.create () in
    Lwt.async begin fun () ->
      let inputs = Lwt_stream.from (fun () -> Lwt_mvar.take ktests_mbox) in
      let* delayed = match wd.options.replay with
        | Delayed -> Lwt_stream.to_list inputs
        | _ -> inputs |> replay_all wd >|= fun () -> []
      in
      let* () = Lwt_stream.of_list delayed |> replay_all wd in
      Lwt_condition.signal replayers_done ();
      Lwt.return ()
    end;
    let stop () =
      let* () = Lwt_mvar.put ktests_mbox None in
      Lwt_condition.wait replayers_done
    and on_ktest ktest =
      Lwt_mutex.with_lock ktests_cache_lock begin fun () ->
        match Hashtbl.find_opt ktests_cache ktest with
        | Some (`Done | `Pending) ->
            Lwt.return ()
        | None ->
            let resbox = Lwt_mvar.create_empty () in
            Hashtbl.add ktests_cache ktest `Pending;
            let* () = Lwt_mvar.put ktests_mbox (Some (ktest, resbox)) in
            let* () =
              Lwt_mutex.unlock ktests_cache_lock;
              let* () = Lwt_mvar.take resbox in
              Lwt_mutex.lock ktests_cache_lock
            in
            Hashtbl.replace ktests_cache ktest `Done;
            Lwt.return ()
      end
    in
    Lwt.return (on_ktest, stop)
  end

let run (wd: _ working_data) =
  let workdir = wd.workspace.workdir in
  let tested_fun = Sc_project.Manager.entrypoint_name wd.project in
  let* seedsdir =          (* directory where some seed tests may be imported *)
    Sc_sys.File.PRETTY.mkdir_in ~dir:workdir "seeds-%s" tested_fun |>
    import_seeds wd
  and* workdir =               (* directory where klee will store its results *)
    Outdir.fresh ~workspace:wd.workspace ~tested_fun
  in
  let klee_dirs = { workdir; seedsdir } in
  let tic = Unix.gettimeofday () in
  let* process, cancel_kill = start_klee wd klee_dirs in
  let* replay_test, stop_replays = start_replayers wd in
  let* stop_uplink = start_ktests_monitoring wd ~replay_test klee_dirs in
  let* () = Sc_sys.Process.join process in
  let time = Unix.gettimeofday () -. tic in
  let () =
    let> oc = workdir / "kleetime" in
    Printf.fprintf oc "%f" time
  in
  Log.info "Klee took %.3fs" time;
  let* () = if wd.options.replay = Delayed then stop_replays () else Lwt.return () in
  let* () = stop_uplink () <&> cancel_kill () in
  let* () = if wd.options.replay <> Delayed then stop_replays () else Lwt.return () in
  Lwt.return ()

let availability_check () =
  Sc_sys.Process.exec_status @@
  Sc_sys.Ezcmd.Std.(make "klee" |>
                    base "size-model" "range" |> (* ensure symbolic size is ok *)
                    key "version" |>
                    to_cmd)

let load () =
  Sc_lib.Tool.register_function For_simple_labels ~name:"klee"
    ~config_section:(Some Config.section)
    ~availability_check ~kind:`Dynamic
    ~setup:begin fun ws ~optional (P p) ->
      let* wd = setup ws ~optional p in
      Lwt.return (fun () -> run wd)
    end
