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
open Sc_sys.File.TYPES
open Types

open Lwt.Syntax
open Lwt.Infix
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_corpus.Validator"))

type 'raw_test t =
  {
    params: 'raw_test validator_params;
    workspace: workspace;
    resdir: dir;
    decoder: 'raw_test Decoder.t;
    store: Sc_store.t;
  }

type 'raw_test ready =
  {
    validator: 'raw_test t;
    launch: test_outcome option Sc_sys.Lwt_task.controlled_launcher;
    replay_with_store_update_exe: [`exe] file;
    replay_for_rte_identification_exe: [`exe] file;
  }

type validation_purpose =
  | For_RTE_identification
  | For_full_validation

(* --- *)

let install_resources_in: workspace:_ -> dir Lwt.t =
  Sc_core.Workspace.install_resources_in @@
  Sc_core.Resource.register_crunched "validator" (module Res)

let make ~workspace decoder store params =
  let* resdir = install_resources_in ~workspace in
  Lwt.return {
    resdir;
    params;
    workspace;
    decoder;
    store;
  }

(* --- *)

(** Validator exit code that is used to detect cases where a test terminates
    successfuly but without bringing any new coverage due to concurrent
    execution of validators. *)
let no_new_coverage_code = 42

(** We may never have to execute tests for which an `sc_assume` fails; in case,
    still detect those cases and ignore the tests. *)
let assumption_failure_code = 43

(** Validator exit code that indicates a failure of the oracle. *)
let oracle_failure_code = 44

let cppflags_for_driver =
  [
    Fmt.str "-D__SC_ASSUMPTION_FAILURE_CODE=%u" assumption_failure_code;
  ]

let cppflags_for_logging validator =
  let log_h = validator.resdir / "sc-raw-validator-log.h" in
  [
    Fmt.str "-D__SC_VALIDATOR_VERBOSITY=%u\
            " validator.params.validator_verbosity;
    "-include"; Sc_sys.File.absname log_h;
  ]

(* --- *)

let sanitizers =
  [
    "address";
    "undefined";
  ]

let sanitizers_opts =
  [
    (* Note: delayed until we properly retrive most RTE summaries *)
    (* "-fno-sanitize-recover=all";              (\* be sure to stop on any error *\) *)
  ]

let sanitizers_flags =
  [
    "-O0";                                          (* better safe than sorry *)
    Fmt.str "-fsanitize=%a" PPrt.Strings.pp_comma_separated sanitizers;
  ]

(* --- *)

let bring_n_compile_test_loader validator =
  let given_loader_h = validator.resdir  / "sc-raw-test-loader.h"
  and given_loader_c = validator.resdir  / "sc-raw-test-loader.c"
  and       loader_h = validator.workspace.workdir / "sc-raw-test-loader.h"
  and       loader_c = validator.workspace.workdir / "sc-raw-test-loader.c" in
  let* () = Sc_sys.Lwt_file.link given_loader_h loader_h in
  let* () = Sc_sys.Lwt_file.link given_loader_c loader_c in
  Sc_C.Cmd.clang_c loader_c
    ~cppflags:["-I"; Sc_sys.File.absname @@ validator.resdir]
    ~cflags:(sanitizers_flags @ sanitizers_opts)

(* --- *)

let bring_n_compile_labelized_file validator =
  let codefile = validator.workspace.workdir / "code-with-labels.c" in
  let driver_h = validator.resdir / "sc-raw-validator-driver.h" in
  let* () = Sc_sys.Lwt_file.link validator.params.labelized_file codefile in
  let* o_file_with_labels =
    Sc_C.Cmd.clang_c codefile
      ~o_suff:"-enabled"
      ~cppflags:(cppflags_for_driver @ cppflags_for_logging validator @
                 ["-U__SC_VALIDATOR_IGNORE_LABELS";
                  "-include"; Sc_sys.File.absname driver_h])
      ~cflags:(sanitizers_flags @ sanitizers_opts)
  and* o_file_without_labels =
    Sc_C.Cmd.clang_c codefile
      ~o_suff:"-disabled"
      ~cppflags:(cppflags_for_driver @ cppflags_for_logging validator @
                 ["-D__SC_VALIDATOR_IGNORE_LABELS";
                  "-include"; Sc_sys.File.absname driver_h])
      ~cflags:(sanitizers_flags @ sanitizers_opts)
  in
  Lwt.return (o_file_with_labels, o_file_without_labels)

(* --- *)

let emit_assignment_to_globals ppf (struct_name, Sc_C.Types.{ glob_vars }) =
  PPrt.pp_lst ~fopen:"@[<h>" ~fsep:";@\n" ~fclose:";@]@\n" ~fempty:""
    (fun ppf (t, v, _) ->
       Sc_C.Printer.pp_typed_assignment ppf (t, v, Fmt.str "%s.%s" struct_name v))
    ppf glob_vars

let emit_effective_inputs ppf (test_struct_name, func_args) =
  PPrt.pp_lst ~fopen:"@[<hv>" ~fsep:",@\n" ~fclose:"@]"
    (fun ppf v -> Fmt.pf ppf "%s.%s" test_struct_name (Sc_C.Defs.var_name v))
    ppf func_args

let emit_oracle_assessment_macro ppf =
  Fmt.pf ppf "#define __sc_assess_oracle(e) \\\
              @\n  do { if (!(e)) exit (%u); } while (0)" oracle_failure_code

let emit_validator_c (type raw_test) ppf (validator: raw_test t) =
  let module Raw_test = (val validator.params.test_repr) in
  let func = validator.params.func_repr
  and max_size = snd @@ Raw_test.size_bounds validator.params.test_struct in
  Fmt.pf ppf
    "@\n#include \"sc-raw-test-loader.h\"\
     @\n#include \"decoder.h\"\
     @\n#include <stdlib.h>\
     @\n#include <stdio.h>\
     @\n#include <string.h>\
     @\n#include <errno.h>\
     @\n#include %S\
     @\n%t\
     @\n\
     @\nunsigned int __sc_buff_commit (void);\
     @\n\
     @\nint main (int argc, char *argv[]) {\
     @\n  struct raw_test raw_test;\
     @\n  %a * p;\
     @\n  __sc_raw_decoder_init ();\
     @\n  if (argc == 1) {\
     @\n    __sc_read_raw_test_from_stdin (&raw_test, %u);\
     @\n  } else if (argc == 2) {\
     @\n    __sc_load_raw_test_file (argv[1], &raw_test);\
     @\n  } else if (argc == 3) {\
     @\n    if (strcmp (argv[1], \"--file\") == 0) {\
     @\n      __sc_load_raw_test_file (argv[2], &raw_test);\
     @\n    } else {\
     @\n      size_t string_size;\
     @\n      errno = 0;\
     @\n      string_size = (size_t) strtoul (argv[1], NULL, 10);\
     @\n      if (errno) {\
     @\n        fprintf (stderr, \"invalid argument \\\"%%s\\\";\"\
     @\n                 \" string length expected\\n\", argv[1]);\
     @\n        exit (EXIT_FAILURE);\
     @\n      }\
     @\n      __sc_load_raw_test_string (argv[2], string_size, &raw_test);\
     @\n    }\
     @\n  } else {\
     @\n    fprintf (stderr, \"invalid number of arguments\\n\");\
     @\n    exit (EXIT_FAILURE);\
     @\n  }\
     @\n  p = __sc_raw_decode (raw_test.data, raw_test.size);\
     @\n  if (p != NULL) {\
     @\n    @[<v>%a%t@]\
     @\n  }\
     @\n  /* Skip deallocations during decoder reset: this may lead to double-free\
     @\n     errors, while we are about to exit anyways. */\
     @\n  __sc_raw_decoder_reset (0);\
     @\n  __sc_unload_raw_test (&raw_test);\
     @\n#ifndef __SC_VALIDATOR_IGNORE_LABELS\
     @\n  if (__sc_buff_commit () == 0) {\
     @\n    __sc_log (\"ign\\n\");\
     @\n    exit (%u); /* \"arbitrary\" code that indicates success but no new coverage */\
     @\n  }\
     @\n#endif\
     @\n  exit (EXIT_SUCCESS);\
     @\n}"
    (Sc_sys.File.absname validator.params.func_header)
    emit_oracle_assessment_macro
    Sc_values.Struct.print validator.params.test_struct
    max_size
    emit_assignment_to_globals ("(*p)", func.func_env)
    (Sc_C.Printer.emit_testcall
       ~oracle_assessment:"__sc_assess_oracle"
       ~entrypoint:validator.params.func_repr
       ?init_func:validator.params.init_func
       ?oracle_func:validator.params.oracle_func
       ~emit_effective_inputs ("(*p)", func.func_args))
    no_new_coverage_code

let provide_validator validator () =
  Lwt.return (validator.workspace.workdir / "replay-raw-test-with-labels",
              validator.workspace.workdir / "replay-raw-test-without-labels")

let gen_n_compile_validator validator decoder_infos () =
  let* store_infos = Sc_store.for_compiled_subprocess validator.store in
  let validate_c = validator.workspace.workdir / "replay-raw-test.c" in
  let* () =
    Log.debug "Generating `%a'" Sc_sys.File.print validate_c;
    let>*% ppf = validate_c in
    Lwt_fmt.fprintf ppf
      "/* Auto-generated file */\
       @\n%a\
       @."
      emit_validator_c validator
  and* loader_o =
    bring_n_compile_test_loader validator
  and* tested_f_with_labels_o,
       tested_f_without_labels_o =
    bring_n_compile_labelized_file validator
  and* replay_with_store_update_exe,
       replay_for_rte_identification_exe =
    provide_validator validator ()
  in
  Let_syntax.both begin
    Sc_C.Cmd.clang_c validate_c
      ~o_suff:"-enabled"
      ~cppflags:(decoder_infos.cppflags @ cppflags_for_logging validator @
                 ["-U__SC_VALIDATOR_IGNORE_LABELS"])
      ~cflags:(sanitizers_flags @ sanitizers_opts) >>=
    Sc_C.Cmd.clang_ld
      ~ldflags:(sanitizers_flags @ decoder_infos.libs @ store_infos.libs)
      ~o_files:[loader_o; tested_f_with_labels_o]
      ~output_filename:(fun _ -> Sc_sys.File.absname replay_with_store_update_exe)
  end begin
    Sc_C.Cmd.clang_c validate_c
      ~o_suff:"-disabled"
      ~cppflags:(decoder_infos.cppflags @ cppflags_for_logging validator @
                 ["-D__SC_VALIDATOR_IGNORE_LABELS"])
      ~cflags:(sanitizers_flags @ sanitizers_opts) >>=
    Sc_C.Cmd.clang_ld
      ~ldflags:(sanitizers_flags @ decoder_infos.libs)
      ~o_files:[loader_o; tested_f_without_labels_o]
      ~output_filename:(fun _ -> Sc_sys.File.absname replay_for_rte_identification_exe)
  end

let setup validator : _ ready Lwt.t =
  let* decoder_infos = Decoder.for_compiled_subprocess validator.decoder in
  let* replay_with_store_update_exe,
       replay_for_rte_identification_exe =
    let workdir = validator.workspace.workdir in
    Sc_sys.Lwt_file.with_lock_in workdir ReadWrite begin fun () ->
      Sc_sys.Lwt_lazy.persist_in ~dir:workdir
        (gen_n_compile_validator validator decoder_infos)
        (provide_validator validator)
    end
  in
  let launch =
    Sc_sys.Lwt_task.with_controlled_parallelism
      ~max_concurrency:validator.params.max_concurrent_validations
  in
  Lwt.return {
    validator;
    launch;
    replay_with_store_update_exe;
    replay_for_rte_identification_exe;
  }

(* --- *)

let validator_log test_ident : (module Ez_logs.T) =
  Ez_logs.subproc @@ Fmt.str "valid:%s" (Str.first_chars test_ident 7)

let stderr ~log:(module Log: Ez_logs.T) =
  `Log Log.LWT.debug

let stdout ~log:(module Log: Ez_logs.T) =
  `Log Log.LWT.debug

(* --- *)

let replay_with_store_update ready_validator ~exec_validator ~log =
  let validator = ready_validator.validator in
  let* store = Sc_store.for_compiled_subprocess validator.store in
  let san_env =
    [|
      "ASAN_OPTIONS=symbolize=0";
      "UBSAN_OPTIONS=symbolize=0";
    |]
  in
  let* res =
    Sc_sys.Process.join =<< exec_validator
      ~exe:ready_validator.replay_with_store_update_exe
      ~env:(Array.concat [store.env; san_env])
      ~stdout:(stdout ~log)
      ~stderr:(stderr ~log)
  in
  match res with
  | Ok () ->
      Lwt.return_ok (Some Covering_label)
  | Error Unix.WEXITED code when code = oracle_failure_code ->
      Lwt.return_ok (Some Oracle_failure)
  | Error Unix.WEXITED code when code = no_new_coverage_code ||
                                 code = assumption_failure_code ->
      Lwt.return_ok None
  | Error _ ->
      Lwt.return_error ()

(* --- *)

let read_log ~f ~fallback log =
  Lwt.catch begin fun () ->
    let<* ic = log in Lwt_io.read_lines ic |> f
  end begin function
    | Sc_sys.File.(MISSING _ | UNIX_ERROR _) -> fallback ()
    | e -> Lwt.reraise e
  end

let remove_log log =
  Lwt.catch begin fun () ->
    Sc_sys.Lwt_file.unlink log
  end begin function
    | Sc_sys.File.(MISSING _ | UNIX_ERROR _) -> Lwt.return ()
    | e -> Lwt.reraise e
  end

let sanitizer_summary_parsers =
  [
    mk_parser "SUMMARY: AddressSanitizer: heap-buffer-overflow (%_s@+0x%Lx)"
      (fun loc -> Heap_buffer_overflow loc);
    mk_parser "SUMMARY: AddressSanitizer: SEGV (%_s@+0x%Lx)"
      (fun loc -> Invalid_memory_address loc);
    mk_parser "SUMMARY: AddressSanitizer: FPE (%_s@+0x%Lx)"
      (fun loc -> Arithmetic_error loc);
  ]

let scan_sanitizer_lines ~log:(module Log: Ez_logs.T) lines =
  Lwt_stream.peek @@
  Lwt_stream.filter_map begin fun line ->
    if String.starts_with ~prefix:"SUMMARY: " line            (* early filter *)
    then (Log.debug "%s" line;
          try_parse sanitizer_summary_parsers line)
    else None
  end lines

let replay_for_rte_identification ready_validator ~exec_validator ~log =
  let validator = ready_validator.validator in
  let workdir = validator.workspace.workdir in
  let asan_log_base = Sc_sys.File.absname @@ workdir / "asan"
  and ubsan_log_base = Sc_sys.File.absname @@ workdir / "ubsan" in
  let san_env =
    [|
      Fmt.str "ASAN_OPTIONS=symbolize=0:log_path=%s" asan_log_base;
      Fmt.str "UBSAN_OPTIONS=symbolize=0:log_path=%s" ubsan_log_base;
    |]
  in
  let* proc =
    exec_validator
      ~exe:ready_validator.replay_for_rte_identification_exe
      ~env:san_env
      ~stdout:`Dev_null
      ~stderr:`Dev_null
  in
  let  pid = Sc_sys.Process.pid  proc in
  let* res = Sc_sys.Process.join proc in
  match res with
  | Ok () ->
      Lwt.return_ok ()                                              (* no RTE *)
  | Error Unix.WEXITED code when code = assumption_failure_code ->
      Lwt.return_ok ()                                       (* no RTE either *)
  | Error _ ->
      let asan_log = Sc_sys.File.PRETTY.assume "%s.%u" asan_log_base pid
      and ubsan_log = Sc_sys.File.PRETTY.assume "%s.%u" ubsan_log_base pid in
      let fallback () = Lwt.return None in
      let scan_sanitizer_lines = scan_sanitizer_lines ~log in
      let* a_err  = read_log asan_log  ~f:scan_sanitizer_lines ~fallback in
      let* ub_err = read_log ubsan_log ~f:scan_sanitizer_lines ~fallback in
      let* () = remove_log asan_log <&> remove_log ubsan_log in
      match ub_err, a_err with
      | None, None ->
          Log.warn "Missing@ sanitizer@ report@ despite@ failing@ validation";
          Lwt.return_ok ()
      | Some err, None | None, Some err ->
          Lwt.return_error (Triggering_RTE err)
      | Some err, Some err' ->
          Log.warn "Found@ multiple@ sanitizer@ reports:@ keeping@ UBSAN's,@ \
                    discarding@ %a" Printer.pp_sanitizer_error_summary err';
          Lwt.return_error (Triggering_RTE err)

let validate ?(purpose = For_full_validation) ~test_ident ready_validator
    ~exec_validator =
  let log = validator_log test_ident in
  ready_validator.launch begin fun () ->
    let* first_stage_res =
      if purpose = For_full_validation
      then replay_with_store_update ready_validator ~exec_validator ~log
      else Lwt.return_error ()                            (* skip first stage *)
    in
    match first_stage_res with
    | Ok (res: test_outcome option) ->
        Lwt.return res
    | Error () ->
        let* second_stage_res =
          replay_for_rte_identification ready_validator ~exec_validator ~log
        in
        match second_stage_res with
        | Ok () ->              (* error code in label definition only: ignore *)
            Lwt.return None
        | Error (e: test_outcome) ->
            Lwt.return (Some e)
  end

let on_error status =
  Lwt.return_error status

let on_success () =
  Lwt.return_ok ()

let validate_raw_test_file ready_validator ?purpose file =
  let* test_ident = Sc_sys.Lwt_file.digest file in
  validate ready_validator ?purpose ~test_ident
    ~exec_validator:begin fun ~exe ~env ~stdout ~stderr ->
      Sc_sys.Process.exec
        [| Sc_sys.File.absname exe; Sc_sys.File.absname file |]
        ~env ~on_success ~on_error ~stdout ~stderr
        ?timeout:ready_validator.validator.params.test_timeout
    end

(** Take care to avoid large amounts of concurrent calls to this function, as
    this may induce an overuse of system pipes.  In these cases, prefer using
    {!validate_raw_test_file}. *)
let validate_raw_test_string ready_validator ?purpose str =
  let test_ident = Digest.to_hex @@ Digest.string str in
  validate ready_validator ?purpose ~test_ident
    ~exec_validator:begin fun ~exe ~env ~stdout ~stderr ->
      let* proc =
        Sc_sys.Process.exec
          [| Sc_sys.File.absname exe |]
          ~env ~on_success ~on_error ~stdout ~stderr
          ?timeout:ready_validator.validator.params.test_timeout
      in
      Sc_sys.Process.stdin_string proc str >>= fun () ->
      Sc_sys.Process.stdin_close proc >>= fun () ->
      Lwt.return proc
    end

(** Warning for {!validate_raw_test_string} applies. *)
let validate_raw_test (type raw_test) (ready_validator: raw_test ready)
    ?purpose (raw_test: raw_test) =
  let module Raw_test = (val ready_validator.validator.params.test_repr) in
  validate_raw_test_string ready_validator ?purpose @@
  Raw_test.Val.to_string raw_test

let show_outcome ?(log_outcome = false) outcome =
  if log_outcome then
    Log.LWT.debug "Test@ outcome:@ %a"
      Fmt.(option ~none:(any "ignored") Printer.pp_test_outcome)
      outcome >>= fun () ->
    Lwt.return outcome
  else
    Lwt.return outcome

(** Warning for {!validate_raw_test_string} applies. *)
let validate_n_share_raw_test (type raw_test) (ready_validator: raw_test ready)
    ~(corpus: raw_test Main.corpus) ~toolname ?purpose ?log_outcome
    (raw_test: raw_test) =
  validate_raw_test ready_validator ?purpose raw_test >>=
  show_outcome ?log_outcome >>= function
  | None ->                                 (* Valid test, but no new coverage *)
      Lwt.return ()
  | Some outcome ->
      Main.share_test' ~toolname ~outcome corpus raw_test

(** Warning for {!validate_raw_test_string} does NOT apply. *)
let validate_n_share_raw_test_file (type raw_test) (ready_validator: raw_test ready)
    ~(corpus: raw_test Main.corpus) ~toolname ?purpose ?log_outcome file =
  let module Raw_test = (val ready_validator.validator.params.test_repr) in
  validate_raw_test_file ready_validator ?purpose file >>=
  show_outcome ?log_outcome >>= function
  | None ->                                 (* Valid test, but no new coverage *)
      Lwt.return ()
  | Some outcome ->
      let* test_str = Sc_sys.Lwt_file.read file in
      Main.share_test' ~toolname ~outcome corpus @@
      Raw_test.Val.of_string ready_validator.validator.params.test_struct
        test_str
