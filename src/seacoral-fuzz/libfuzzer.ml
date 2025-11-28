(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES
open Sc_core.Types
open Sc_project.Types
open Format

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax

let toolname = "libfuzzer"

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create ~doc:"Logs of libfuzzer" "Sc_fuzz.Libfuzzer"))

type opt =
  {
    seed: int;                                            (* unspecified if 0 *)
    init_corpus_size: int;
    init_corpus_with_uniform_bytes: char list;
    runs: int;
    timeout: float;                    (* max fuzzing time, in seconds *)
    triage_timeout: float;             (* max triage time, in seconds *)
    micro_timeout: float;              (* single-test timeout, in seconds *)
    max_starvation_time: int;          (* == max seconds without valid inputs *)
    use_counters: bool;
    use_cmp: bool;
    labels_only: bool;
  }

type 'raw_test working_data =
  {
    workspace: Sc_core.Types.workspace [@warning "-unused-field"];
    codefile: [`C | `labelized] file;
    fuzzer_exe: [`exe] file;
    project: 'raw_test Sc_project.Types.project;
    validator: 'raw_test Sc_corpus.Validator.ready;
    resdir: dir [@warning "-unused-field"];
    opt: opt;
  } [@@warning "-unused-field"]                            (* for ocaml < 5.1 *)

let config_section =
  let default =
    {
      seed = 0;
      init_corpus_size = 10;
      init_corpus_with_uniform_bytes = ['\x00'; '\x01'];
      runs = -1;
      use_counters = false;
      use_cmp = true;
      timeout = 30.;
      triage_timeout = 0.;
      micro_timeout = 1.;
      max_starvation_time = 60;
      labels_only = false;
    }
  in
  let byte_parse str =
    if String.starts_with ~prefix:"'" str &&
       String.ends_with ~suffix:"'"  str
    then Scanf.sscanf str "%C%!" Fun.id
    else Scanf.sscanf ("'"^str^"'") "%C%!" Fun.id
  and byte_unparse char = Fmt.str "%s" (String.make 1 char) in
  let byte_check str =
    try ignore @@ byte_parse str; true with Scanf.Scan_failure _ -> false
  in
  Sc_config.Section.define toolname ~default ~entries:Sc_config.Eztoml.[
      int
        ~key:"seed"
        ~doc:"Seed to pass to libfuzzer.  Default is %a, which instructs \
              libfuzzer to generate a seed internally."
        ~default:0
        ~runtime:true
        (fun c seed -> { c with seed })
        (fun c -> c.seed);
      string_list
        ~key:"init-corpus-with-uniform-bytes"
        ~doc:"Whether to include inputs that only consist of bytes of the given \
              values into the initial corpus (default: %a)"
        ~check:(List.for_all byte_check)
        ~check_descr:"single-byte@ characters@ expected@ (e.g.@ \"\\x00\",@ \
                      \"\\001\")"
        ~default:(List.map byte_unparse default.init_corpus_with_uniform_bytes)
        ~runtime:true
        (fun c b -> { c with init_corpus_with_uniform_bytes = List.map byte_parse b })
        (fun c -> List.map byte_unparse c.init_corpus_with_uniform_bytes);
      bool
        ~key:"init-corpus-with-nulls"
        ~doc:"Whether to include an input that only consists of null bytes into \
              the initial corpus (default: %a); equivalent to specifying '\\000' \
              in `init-corpus-with-uniform-bytes`"
        ~default:(List.mem '\x00' default.init_corpus_with_uniform_bytes)
        ~runtime:true
        (fun c b ->
           if b = List.mem '\x00' c.init_corpus_with_uniform_bytes then c else
             let insert_or_remove =
               if b
               then List.cons '\x00'
               else List.filter (fun x -> x <> '\x00')
             in
             { c with init_corpus_with_uniform_bytes =
                        insert_or_remove c.init_corpus_with_uniform_bytes })
        (fun c -> List.mem '\x00' c.init_corpus_with_uniform_bytes);
      strictly_positive_int
        ~key:"init-corpus-size"
        ~doc:"Size of the initial input corpus (default: %a; must be strictly \
              positive)"
        ~default:default.init_corpus_size
        ~runtime:true
        (fun c i -> { c with init_corpus_size = i })
        (fun c -> c.init_corpus_size);
      int
        ~key:"runs"
        ~doc:"Number of individual test runs (default: %a; a strictly negative \
              value indicates an infinite number of runs)"
        ~default:default.runs
        ~runtime:true
        (fun c runs -> { c with runs })
        (fun c -> c.runs);
      bool
        ~key:"use-counters"
        ~doc:"Whether libfuzzer's coverage counters may be used as features \
              (default: %a; using counters may increase the amount of generated \
              test cases)"
        ~default:default.use_counters
        ~runtime:true
        (fun c use_counters -> { c with use_counters })
        (fun c -> c.use_counters);
      bool
        ~key:"use-cmp"
        ~doc:"Whether tracing of comparison instructions may be used to guide \
              mutations (default: %a; using this feature may increase the amount \
              of generated test cases)"
        ~default:default.use_cmp
        ~runtime:true
        (fun c use_cmp -> { c with use_cmp })
        (fun c -> c.use_cmp);
      bool
        ~key:"labels-only"
        ~doc:"Whether to restrict libfuzzers' sensitivity to labels instead of \
              to its own counters (default: %a; `use-counters` and `use-cmp` \
              have no effect when used in combination with this option)"
        ~default:default.labels_only
        (fun c labels_only -> { c with labels_only })
        (fun c -> c.labels_only);
      float
        ~key:"timeout"
        ~doc:"Total time to spend while fuzzing (in seconds, default: %a; 0 \
              means run indefinitely)"
        ~default:default.timeout
        ~runtime:true
        (fun c timeout -> { c with timeout })
        (fun c -> c.timeout);
      float
        ~key:"triage-timeout"
        ~doc:"Total time to spend while in each triage phase (in seconds, \
              default: %a; 0 means run indefinitely)"
        ~default:default.triage_timeout
        ~runtime:true
        (fun c triage_timeout -> { c with triage_timeout })
        (fun c -> c.triage_timeout);
      float
        ~key:"micro-timeout"
        ~doc:"Single-test timeout (in seconds, default: %a)"
        ~default:default.micro_timeout
        ~runtime:true
        (fun c micro_timeout -> { c with micro_timeout })
        (fun c -> c.micro_timeout);
      strictly_positive_int
        ~key:"max-starvation-time"
        ~doc:"Maximum number of seconds to wait for valid inputs from the corpus \
              in case no randomly generated seed is valid (in seconds, default: \
              %a)"
        ~default:default.max_starvation_time
        ~runtime:true
        (fun c t -> { c with max_starvation_time = t })
        (fun c -> c.max_starvation_time);
    ]


(* let filter_sanitizers = *)
(*   [ *)
(*     (\* Using no sanitizer for now. *\) *)
(*     "address"; *)
(*     "undefined"; *)
(*   ] *)

let sanitizers_opts =
  [
    (* "-fsanitize-coverage=trace-cmp"; *)
    "-fno-sanitize-recover=all";              (* be sure to stop on any error *)
  ]

let pass_driver_cxx ~resdir =
  [
    "-include"; Sc_sys.File.absname (resdir / "sc_libfuzzer_harness.cxx");
  ]

let include_driver_h ~resdir =
  [
    "-include"; Sc_sys.File.absname @@ resdir / "sc_fuzz_driver.h";
  ]

(* let use_store_def = *)
(*   [ *)
(*     "-D__SC_FUZZ_HAVE_SHARED_STORE=1"; *)
(*   ] *)

and enable_optional_labels_hack { labels_only; _ } =
  [
    asprintf "-D__SC_FUZZ_ENABLE_LABELSONLY_HACK=%B" labels_only;
  ]

(* let force_enable_labels_hack =                         (\* always enable *\) *)
(*   [ *)
(*     asprintf "-D__SC_FUZZ_ENABLE_LABELSONLY_HACK=%B" true; *)
(*   ] *)

let clang_c ~cover ?(sanitizers = [])
    ?(cflags = ["-g"; "-Wno-unknown-attributes"]) =
  let cflagsx =
    if cover
    then (asprintf "-fsanitize=%a" Basics.PPrt.Strings.pp_comma_separated
            ("fuzzer-no-link" :: sanitizers)) :: sanitizers_opts
    else []
  in
  Sc_C.Cmd.clang_c ~cflags:(cflags @ cflagsx)

let clang_cxx ~cover ?(sanitizers = []) ?(cxxflags = ["-g"]) =
  let cxxflagsx =
    if cover
    then (asprintf "-fsanitize=%a" Basics.PPrt.Strings.pp_comma_separated
            ("fuzzer-no-link" :: sanitizers)) :: sanitizers_opts
    else []
  in
  Sc_C.Cmd.clang_cxx ~cxxflags:(cxxflags @ cxxflagsx)

let clang_libfuzzer ?(sanitizers = []) ?(libs = []) harness_o tested_o =
  Sc_C.Cmd.clang_ld
    ~ld_cmd:(Lazy.force Sc_C.Build_tools.config).clangxx_path
    ~ldflags:([
        asprintf "-fsanitize=%a" Basics.PPrt.Strings.pp_comma_separated
          ("fuzzer" :: sanitizers);
      ] @ libs)
    ~o_files:[tested_o]
    harness_o

(** [setup ?dry workspace project] prepares the given workspace to operate on
    the test generation problem of [project].  This notably generates the code
    of a harness for libfuzzer.  Returns a working state from which that harness
    can be compiled and used for generating test-cases. *)
let setup ?dry workspace project : _ working_data Lwt.t =
  let opt = Sc_config.Section.get config_section in
  let* resdir = Fuzzing.install_resources_in ~workspace in
  let labelized_file = project.label_data.labelized_file in
  let codefile = workspace.workdir / "code-with-labels.c" in
  Sc_sys.File.link ?dry labelized_file codefile;
  let* fuzzer_exe =
    let* decoder = Sc_corpus.Decoder.for_compiled_subprocess project.decoder in
    let* tested_f_o =
      clang_c codefile ~cover:true ~o_suff:"-fuzzer"
        ~cppflags:(include_driver_h ~resdir)
    in
    let* fuzzer_o =
      Fuzzing.make_harness_cxx ?dry (module Log) project
        ~outdir:workspace.workdir >>=
      clang_cxx ~cover:false ~o_suff:"-fuzzer"
        ~cppflags:(decoder.cppflags @
                   pass_driver_cxx ~resdir @
                   enable_optional_labels_hack opt)
    in
    clang_libfuzzer fuzzer_o tested_f_o ~libs:decoder.libs
  and* validator =
    Sc_corpus.Validator.setup project.validator
  in
  Lwt.return { workspace; codefile; fuzzer_exe;
               project; validator; resdir; opt }

let triage_error exe status =
  Log.LWT.warn "Fuzzer `%s' exited with status %a while triaging"
    exe Sc_sys.Process.pp_unix_status status

let force_bounded_runs wd =
  let runs = wd.opt.runs and max_total_time = int_of_float wd.opt.timeout in
  if runs < 0 && max_total_time <= 0 then begin
    Log.warn "Fuzzing options runs=%d and timeout=%d indicate fuzzing for an \
              indefinite time: forcing runs=%u\
             " runs max_total_time Int.max_int;
    Int.max_int
  end else
    wd.opt.runs

(* Log modules for reporting outputs of fuzzer and filter sub-processes *)
module Log_fuzzer: Ez_logs.T = (val Ez_logs.subproc "fuzz-fuzzer")
module Log_filter: Ez_logs.T = (val Ez_logs.subproc "fuzz-filter")

let succ_if_nonzero = function
  | 0. -> None
  | i -> Some (i +. 1.)

(* Note: [*dir]s must be siblings of [exe] *)
let triage (type raw_test) (wd: raw_test working_data) ?env ?(mortal = true)
    ~logdir ~opname exe outdir indirs =
  let module Raw_test = (val wd.project.params.test_repr) in
  let (module Subproc_log: Ez_logs.T), exe = exe in
  let test_struct = Sc_corpus.test_struct wd.project.corpus in
  let rel_outdir = Sc_sys.File.basename outdir
  and rel_indirs = List.map Sc_sys.File.basename indirs
  and dir = Sc_sys.File.dir exe
  and exe = Sc_sys.File.basename exe in
  let { run_num; _ } = Sc_project.Manager.run_info wd.project in
  let crashdir =
    Sc_sys.File.PRETTY.mkdir_in ~dir "crashes-on-%s" opname
  and out_file =
    Sc_sys.File.PRETTY.not_in ~dir:logdir "%u-%s.out" run_num opname
  and err_file =
    Sc_sys.File.PRETTY.not_in ~dir:logdir "%u-%s.err" run_num opname
  in
  let out_fd =
    Sc_sys.File.descriptor out_file [O_WRONLY; O_APPEND; O_CREAT] 0o644
  and err_fd =
    Sc_sys.File.descriptor err_file [O_WRONLY; O_APPEND; O_CREAT] 0o644
  in
  Log.K.debug begin fun p ->
    p "Standard output for %s will go into `%a'" opname Sc_sys.File.print out_file;
    p "Standard error for %s will go into `%a'" opname Sc_sys.File.print err_file;
  end;
  let* proc =
    Sc_sys.Process.exec
      (Array.of_list ([
           asprintf "./%s" exe;
           asprintf "-max_total_time=%i" (int_of_float wd.opt.triage_timeout);
           asprintf "-timeout=%i" (int_of_float wd.opt.micro_timeout);
           asprintf "-max_len=%d" (snd @@ Raw_test.size_bounds test_struct);
           asprintf "-artifact_prefix=%s/" (Sc_sys.File.basename crashdir);
           "-merge=1";
         ] @ [ rel_outdir ]
           @ rel_indirs))
      ?env
      ~cwd:(Sc_sys.File.absname dir)
      ~stdout:(`FD_move out_fd)
      ~stderr:(`FD_move err_fd)
      ?timeout:(succ_if_nonzero wd.opt.triage_timeout)
      ~on_success:Lwt.return
      ~on_error:(triage_error exe)
  in
  if not mortal
  then Sc_sys.Process.join proc
  else begin
    let* cancel_kill =
      Sc_store.on_termination wd.project.store
        ~h:(fun _ -> Sc_sys.Process.kill ~delay:1. Sys.sigint proc)
    in
    let* () = Sc_sys.Process.join proc in
    cancel_kill ()
  end

let init_seeds wd ?env ~logdir exe indir seedsdir =
  let* { num_imported; num_generated } =
    Sc_corpus.Seeding.init_seeds wd.project.params.test_repr seedsdir
      ~corpus:wd.project.corpus
      ~init_corpus_size:wd.opt.init_corpus_size
      ~init_corpus_with_uniform_bytes:wd.opt.init_corpus_with_uniform_bytes
  in
  if num_generated > 0 then
    Log.LWT.debug "Merging@ seeds" >>= fun () ->
    triage wd ?env ~logdir exe indir ~opname:"seed-triage" [seedsdir]
  else if num_imported > 0 then              (* copy from seedsdir into indir *)
    Sc_sys.Lwt_file.files_of_dir seedsdir |>
    Lwt_stream.iter_p (Sc_sys.Lwt_file.move_in ~dir:indir)
  else
    Lwt.return ()

let as_int x = if x then 1 else 0

let exec_fuzzer (type raw_test) (wd: raw_test working_data) ?env exe outdir
    seedsdir crashdir =
  let module Raw_test = (val wd.project.params.test_repr) in
  let (module Subproc_log: Ez_logs.T), exe = exe in
  let test_struct = Sc_corpus.test_struct wd.project.corpus in
  let rel_outdir = Sc_sys.File.basename outdir
  and rel_seedsdir = Sc_sys.File.basename seedsdir
  and rel_crashdir = Sc_sys.File.basename crashdir
  and dir = Sc_sys.File.dirname exe
  and exe = Sc_sys.File.basename exe in
  let runs = force_bounded_runs wd in
  Log.debug "Starting fuzzer `%s' in directory `%s'" exe dir;
  let rec aux ~oncrash =
    let* proc =
      Sc_sys.Process.exec
        (Array.of_list ([
             asprintf "./%s" exe;
             asprintf "-seed=%d" wd.opt.seed;
             asprintf "-runs=%d" runs;
             asprintf "-max_len=%d"
               (snd @@ Raw_test.size_bounds test_struct);
             "-fork=1";
             "-ignore_crashes=1";
             asprintf "-artifact_prefix=%s/" rel_crashdir;
             asprintf "-use_counters=%i" (as_int wd.opt.use_counters);
             asprintf "-use_cmp=%i" (as_int wd.opt.use_cmp);
             asprintf "-max_total_time=%i" (int_of_float wd.opt.timeout);
             asprintf "-timeout=%i" (int_of_float wd.opt.micro_timeout);
             (* asprintf "-minimize_crash=%i" (as_int oncrash); *)
             (* "-use_value_profile=1"; *)
             rel_outdir;
           ] @ if oncrash then [] else [
            rel_seedsdir;
          ]))
        ?env ~cwd:dir
        ~stdout:(`Log Subproc_log.LWT.debug)
        ~stderr:(`Log Subproc_log.LWT.debug)
        ?timeout:(succ_if_nonzero wd.opt.timeout)
        ~on_success:Lwt.return
        ~on_error:(function
            | Unix.WEXITED 77 -> aux ~oncrash:true
            | _ -> Lwt.return ())          (* already logged in Sc_sys.Process *)
    in
    let* cancel_kill =
      Sc_store.on_termination wd.project.store
        ~h:(fun _ -> Sc_sys.Process.kill ~delay:1. Sys.sigint proc)
    in
    let* () = Sc_sys.Process.join proc in
    cancel_kill ()
  in
  aux ~oncrash:false

(* Note: as we're manually limiting the submission of crashes (see (i) below),
   we may still be able to use the validation path that incurrs some more pipe
   usage as it reads the file only once.  If we get [EMFILE] Unix errors we may
   need to switch to the slightly less efficient variant where the raw test file
   is read several times. *)
let validate_n_share_test (type r) purpose (wd: r working_data) file =
  let module Raw_test = (val wd.project.params.test_repr) in
  let* test_str = Sc_sys.Lwt_file.read file in
  Sc_corpus.Validator.validate_n_share_raw_test wd.validator
    ~corpus:wd.project.corpus ~toolname ~purpose @@
  Raw_test.Val.of_string wd.project.params.test_struct test_str

let start_sharing_tests_from dir wd =
  Sc_sys.Lwt_watch.ASYNC.monitor_dir dir
    ~on_close:(validate_n_share_test For_full_validation wd)

let san_env =
  [|
    "ASAN_OPTIONS=symbolize=0";
    "UBSAN_OPTIONS=symbolize=0";
  |]

let run wd =
  let fuzzer_exe = (module Log_fuzzer: Ez_logs.T), wd.fuzzer_exe in
  let dir = Sc_sys.File.dir wd.codefile in
  let seedsdir = Sc_sys.File.mkdir_in ~dir "seeds"
  and indir = Sc_sys.File.mkdir_in ~dir "working-inputs"
  and crashdir = Sc_sys.File.mkdir_in ~dir "crashes"
  and logdir = Sc_sys.File.mkdir_in ~dir "logs" in             (* triage logs *)
  let* () =
    Sc_sys.Lwt_file.unlink_files_of_dir indir                  (* empty indir *)
  and* () =
    Sc_sys.Lwt_file.unlink_files_of_dir crashdir            (* empty crashdir *)
  in
  let* stop_indir_monitoring = start_sharing_tests_from indir wd in
  let tic = Unix.gettimeofday () in
  let rec try_fuzzing ?(attempt = 0) () =
    let* empty = Sc_sys.Lwt_file.dir_is_empty indir in
    if empty && attempt >= wd.opt.max_starvation_time then
      Log.LWT.warn "No working seed found: fuzzing skipped."
    else if empty then
      Log.LWT.warn "No working seed found: next retry in 1 second." >>= fun () ->
      Lwt_unix.sleep 1.0 >>=
      try_fuzzing ~attempt:(succ attempt)
    else                                     (* execute the main fuzzing loop *)
      exec_fuzzer ~env:san_env wd fuzzer_exe indir seedsdir crashdir
  in
  Lwt.finalize begin fun () ->             (* nominal *)
    (* Put "interesting" seeds in `indir` *)
    init_seeds ~env:san_env wd ~logdir fuzzer_exe indir seedsdir >>=
    try_fuzzing
  end begin fun () ->
    let* () = stop_indir_monitoring ()
    and* () =
      Sc_sys.Lwt_file.files_of_dir crashdir |>
      Lwt_stream.iter_n ~max_concurrency:16      (* Note: arbitrary limit (i) *)
        (validate_n_share_test For_RTE_identification wd)
    in
    (* TODO: just compute stats from corpus and store info. No need to do that
       in this way.  Here we may just concentrate on timing. *)
    let time = Unix.gettimeofday () -. tic in (* Note: includes RTE validation *)
    Sc_project.Manager.report_tool_status wd.project ~toolname (Ok ())
      ~tests_generated:0 ~elapsed_time:time
  end

let availability_check () : bool Lwt.t =
  (* Just check clang version >= 6 (?) *)
  Sc_sys.Process.shell_status @@
  Sc_C.Cmd.clang " --version | head -n 1 | grep -v -q 'version [0-5]\\.'"

let load () =
  Sc_lib.Tool.register_function For_simple_labels ~name:toolname
    ~config_section:(Some config_section)
    ~availability_check ~kind:`Dynamic
    ~setup:begin fun ws ~optional (P p) ->
      let* wd = setup ws ~dry:optional p in
      Lwt.return (fun () -> run wd)
    end
