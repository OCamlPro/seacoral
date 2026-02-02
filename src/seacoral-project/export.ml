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
open Sc_corpus.Types
open Sc_sys.File.TYPES

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

(* --- *)

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_project.Export"))

(* --- *)

type options =
  {
    split: bool;
    malloc_heapside_memory: bool;
  }

let config_section =
  let default =
    {
      split = false;
      malloc_heapside_memory = true;
    }
  in
  Sc_config.Section.define "export" ~default ~entries:Sc_config.Eztoml.[
      bool
        ~key:"split-suite"
        ~doc:"Whether to put individual tests in separate C files, or to combine \
              them. Defaults to %a"
        ~default:default.split
        ~runtime:false
        (fun c b -> { c with split = b })
        (fun c -> c.split);
      bool
        ~key:"malloc-heapside-memory"
        ~doc:"Use `malloc` to allocate heap-side memory in generated C \
              code. When false, heap-side memory is allocated \
              statically. Defaults to %a. Forced to true in combined testsuites"
        ~default:default.malloc_heapside_memory
        ~runtime:true            (* not so sure; not "project-wide" either... *)
        (fun c b -> { c with malloc_heapside_memory = b })
        (fun c -> c.malloc_heapside_memory);
    ]

(* --- *)

let oracle_verdict_assessment_preproc_flag = "SC_ASSESS_ORACLE_VERDICT"
let exit_status = "__sc_exit_status"
let oracle_assessment = "__sc_assess_oracle"
let oracle_failure_counter_name = "__sc_oracle_failures"

let time_string time =
  Fmt.str "%.3f" time |> String.map (function '.' -> 's' | c -> c)

(* Returns the test driver file name.  For `lreplay` not to be disturbed, every
   `.` is replaced with an underscore `_`. *)
let assume_test_file ~project { serialnum; toolname; creatime; outcome; _ }
  : [`C] file =
  let run_num = project.config.project_run.run_num in
  let run_ref_time = Run.ref_time project.config.project_run in
  let basename =
    Printf.sprintf "%04u-%s-@%u" serialnum toolname run_num |>
    String.map (function '.' -> '_' | c -> c)
  in
  let time = time_string (creatime -. run_ref_time) in
  match outcome, project.faildir with
  | Covering_label, _ ->
      Sc_sys.File.PRETTY.assume_in ~dir:project.covdir "%s+%s.c" basename time
  | Triggering_RTE _, _ ->
      Sc_sys.File.PRETTY.assume_in ~dir:project.rtedir "%s+%s.c" basename time
  | Oracle_failure, Some faildir ->
      Sc_sys.File.PRETTY.assume_in ~dir:faildir "%s+%s.c" basename time
  | Oracle_failure, None ->
      raise @@ Sc_corpus.Types.INTERNAL_ERROR (Unexpected_outcome outcome)

let emit_exit_status_macro ~with_main ppf =
  if with_main then
    Fmt.pf ppf "@\n#define %s EXIT_SUCCESS" exit_status

let emit_oracle_assessment_body ppf =
  Fmt.pf ppf
    "#include <stdio.h> /* for fprintf */\
     @\nstatic unsigned %s = 0;\
     @\n# define %s (%s == 0 ? EXIT_SUCCESS : EXIT_FAILURE)\
     @\n# define %s(e) \\\
     @\n  do { \\\
     @\n    if (!(e)) { \\\
     @\n      %s ++; \\\
     @\n      fprintf (stderr, \"%%s: oracle failure\\n\", __func__); \\\
     @\n    } \\\
     @\n  } while (0)"
    oracle_failure_counter_name exit_status
    oracle_failure_counter_name oracle_assessment
    oracle_failure_counter_name

let emit_testsuite_header ~with_main ppf project =
  let pp_needed_stddefs_includes ppf =
    if project.params.cil_typing_info.stdbool_needed then
      Fmt.pf ppf
        "@\n#include <stdbool.h> /* for _Bool */"
  and pp_oracle_assessment_header_macros ppf =
    if project.params.oracle_func <> None then
      Fmt.pf ppf
        "@\n#ifdef %s\
         @\n%t
         @\n#else\
         %t\
         @\n# define %s(e) (void) (e)\
         @\n#endif"
        oracle_verdict_assessment_preproc_flag
        emit_oracle_assessment_body
        (emit_exit_status_macro ~with_main)
        oracle_assessment
    else
      emit_exit_status_macro ~with_main ppf
  in
  Fmt.pf ppf
    "#include <string.h> /* for memcpy */\
     @\n#include <stdlib.h> /* for malloc */\
     @\n#include <assert.h>";
  pp_needed_stddefs_includes ppf;
  pp_oracle_assessment_header_macros ppf

let sufficient_generated_headers project =
  [ project.types_header; project.func_header ]

let sufficient_cppflags project =
  List.concat_map (fun h -> ["-include"; Sc_sys.File.absname h]) @@
  sufficient_generated_headers project

let emit_c_test_comment ~project ppf
    { toolname; creatime; crearun = run_num; outcome; _ } =
  let run_ref_time = Run.ref_time_of ~run_num project.config.project_run in
  let time = time_string (creatime -. run_ref_time) in
  Fmt.pf ppf "/* Found by %s after %s in run %u */" toolname time run_num;
  Fmt.pf ppf "@\n";
  Fmt.pf ppf "/* Outcome: %a */" Sc_corpus.Printer.pp_test_outcome outcome

let emit_test_file (type raw_test) ~(project: raw_test project) ~metadata ppf
    raw_test =
  let export_options = Sc_config.Section.get config_section in
  let module Raw_test = (val project.params.test_repr) in
  let module C_printer = Raw_test.Val.Printer in
  let C_printer.{ pp_heap; pp_globals; pp_locals } =
    let globals = project.params.func_repr.func_env.glob_vars
    and locals = project.params.func_repr.func_args in
    if export_options.malloc_heapside_memory
    then C_printer.instructions_as_c_code ~globals ~locals
      @@ Raw_test.Val.fields_as_c_allocations raw_test
    else C_printer.literal_memory_as_c_code ~globals ~locals
      @@ Raw_test.Val.fields_as_c_literals raw_test
  in
  Lwt_fmt.fprintf ppf
    "%a\
     @\n\
     @\n%t\
     @\nint main () {\
     @\n  @[%a@]\
     @\n  /* Globals, if any */\
     @\n  @[%t@]\
     @\n  /* Effective argument(s), if any */\
     @\n  @[%t@]\
     @\n  @[<v>%t@]\
     @\n  return %s;\
     @\n}\
     @."
    (emit_testsuite_header ~with_main:true) project
    (pp_heap ~static:true)
    (emit_c_test_comment ~project) metadata
    pp_globals
    pp_locals
    (Printer.C.emit_testcall                      (* force oracle if provided *)
       { project.params with seek_oracle_failures = true }
       ~oracle_assessment
       ~emit_effective_inputs:Sc_C.Printer.pp_vars
       project.params.func_repr.func_args)
    exit_status

(** Generates the C test files corresponding to the digest in argument *)
let write_test_file (type raw_test) ~(project: raw_test project)
    ({ metadata; raw; _ }: raw_test test_view) =
  let module Raw_test = (val project.params.test_repr) in
  let out_file = assume_test_file ~project metadata in
  Lwt.catch begin fun () ->
    let* () =
      let* exists = Sc_sys.Lwt_file.exists out_file in
      if exists then
        Log.LWT.info "@[Test@ driver@ `%a'@ already@ exists@]"
          Sc_sys.File.print out_file
      else
        let* raw_test = Lazy.force raw in
        let>*% ppf = out_file in
        let* () = emit_test_file ~project ~metadata ppf raw_test in
        Log.LWT.info "@[<hov>Wrote@ test@ driver@ `%a'@ with@ inputs@;@[%a@]@]"
          Sc_sys.File.print out_file
          (Fmt.styled `Cyan @@
           Fmt.styled `Faint @@ Raw_test.Val.print) raw_test
    in
    Lwt.return ({ file = out_file; metadata }: individual_test_in_testsuite)
  end begin fun exn ->
    Log.err "Error while writing test driver %a" Sc_sys.File.print out_file;
    Lwt.reraise exn
  end

type _ output_for_outcome =
  | Covering_label_outcomes: [`C] file output_for_outcome
  | Triggering_RTE_outcomes: [`C] file output_for_outcome
  | Oracle_failure_outcomes: [`C] file option output_for_outcome

(** File name for combined testsuites. *)
let assume_testsuite_file (type a) ~project
    (output_for_outcome: a output_for_outcome) : a =
  match output_for_outcome, project.faildir with
  | Covering_label_outcomes, _ ->
      project.covdir  / "testsuite.c"
  | Triggering_RTE_outcomes, _ ->
      project.rtedir  / "testsuite.c"
  | Oracle_failure_outcomes, Some faildir ->
      Some (faildir / "testsuite.c")
  | Oracle_failure_outcomes, None ->
      None

let emit_c_test_name ppf test_metadata =
  Fmt.pf ppf "test_%u" test_metadata.serialnum

let emit_testsuite_file (type raw_test) ~(project: raw_test project)
    ~with_main ppf (test_views: raw_test test_view list) =
  let module Raw_test = (val project.params.test_repr) in
  let module C_printer = Raw_test.Val.Printer in
  let* () =
    Lwt_fmt.fprintf ppf "%a@\n" (emit_testsuite_header ~with_main) project
  in
  let* () =
    Lwt_list.iter_s begin fun { metadata; raw; _ } ->
      let* raw_test = Lazy.force raw in
      let C_printer.{ pp_heap; pp_globals; pp_locals } =
        let globals = project.params.func_repr.func_env.glob_vars
        and locals = project.params.func_repr.func_args in
        C_printer.instructions_as_c_code ~globals ~locals @@
        Raw_test.Val.fields_as_c_allocations raw_test
      in
      ignore pp_heap; (* always a no-op (cf Sc_values...instructions_as_c_code) *)
      Lwt_fmt.fprintf ppf
        "@\nvoid %a () {\
         @\n  @[%a@]\
         @\n  /* Globals, if any */\
         @\n  @[%t@]\
         @\n  /* Effective argument(s), if any */\
         @\n  @[%t@]\
         @\n  @[<v>%t@]\
         @\n}\
         @\n"
        emit_c_test_name metadata
        (emit_c_test_comment ~project) metadata
        pp_globals
        pp_locals
        (Printer.C.emit_testcall                  (* force oracle if provided *)
           { project.params with seek_oracle_failures = true }
           ~oracle_assessment
           ~emit_effective_inputs:Sc_C.Printer.pp_vars
           project.params.func_repr.func_args)
    end test_views
  in
  if with_main then
    Lwt_fmt.fprintf ppf
      "@\nint main () {\
       @\n  %a\
       @\n  return %s;\
       @\n}\
       @."
      Fmt.(vbox @@
           list ~sep:sp (using (fun { metadata; _ } -> metadata)
                           (emit_c_test_name ++ any " ();")))
      test_views
      exit_status
  else
    Lwt_fmt.flush ppf

(** Generates the C test files corresponding to the digest in argument *)
let write_testsuite_file (type raw_test) ~(project: raw_test project)
    ?(with_main = false) (test_views: raw_test test_view list) out_file =
  Lwt.catch begin fun () ->
    let* () =
      let>*% ppf = out_file in
      let* () = emit_testsuite_file ~project ~with_main ppf test_views in
      Log.LWT.info "@[Wrote@ full@ testsuite@ `%a'@]"
        Sc_sys.File.print out_file
    in
    let metadata = List.map (fun t -> t.metadata) test_views in
    Lwt.return ({ file = out_file; metadata }: combined_tests_in_testsuite)
  end begin fun exn ->
    Log.err "Error while writing test driver %a" Sc_sys.File.print out_file;
    Lwt.reraise exn
  end

let compare_tests t1 t2 =
  Int.compare t1.metadata.serialnum t2.metadata.serialnum

let write_testsuite ?exclude project =
  let export_options = Sc_config.Section.get config_section in
  let collect_failures = project.params.seek_oracle_failures in
  let* tests =
    Lwt_stream.to_list @@
    Sc_corpus.existing_tests ?exclude project.corpus
  in
  let tests = List.sort (fun t1 t2 -> - compare_tests t1 t2) tests in (* reverse *)
  if export_options.split then
    let* cov, rte, fail =
      Lwt_list.fold_left_s begin fun (cov, rte, fail) test_view ->
        let* file = write_test_file ~project test_view in
        Lwt.return @@
        match test_view.metadata.outcome with
        | Covering_label -> file :: cov, rte, fail
        | Triggering_RTE _ -> cov, file :: rte, fail
        | Oracle_failure -> cov, rte, file :: fail
      end ([], [], []) tests
    in
    let fail = if collect_failures then Some fail else None in
    Lwt.return @@ Split_testsuite {  cov;  covdir  = project.covdir;
                                     rte;  rtedir  = project.rtedir;
                                     fail; faildir = project.faildir }
  else
    let cov, rte, fail =
      List.fold_left begin fun (cov, rte, fail) test_view ->
        match test_view.metadata.outcome with
        | Covering_label -> test_view :: cov, rte, fail
        | Triggering_RTE _ -> cov, test_view :: rte, fail
        | Oracle_failure -> cov, rte, test_view :: fail
      end ([], [], []) tests
    in
    let* cov =
      assume_testsuite_file ~project Covering_label_outcomes |>
      write_testsuite_file ~project ~with_main:true cov
    and* rte =
      assume_testsuite_file ~project Triggering_RTE_outcomes |>
      write_testsuite_file ~project rte
    and* fail =
      assume_testsuite_file ~project Oracle_failure_outcomes |> function
      | Some file ->
          Lwt.return_some =<< write_testsuite_file ~project fail file
      | None ->
          Lwt.return_none
    in
    Lwt.return @@ Combined_testsuite {  cov;  covdir  = project.covdir;
                                        rte;  rtedir  = project.rtedir;
                                        fail; faildir = project.faildir }

let assume_testsuite project =                                     (* for now *)
  write_testsuite project
