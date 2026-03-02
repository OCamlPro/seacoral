(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(* Post processing of test cases with E-ACSL *)

open Sc_project.Types

open Lwt.Infix
open Lwt.Syntax

(* --- *)

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_postproc.Eacsl"))

module TYPES = struct

  type options =
    {
      eacsl_enabled: bool;
      eacsl_mode: eacsl_postproc;
      eacsl_gcc_path: string;
    }
  and eacsl_postproc =
    | Sequential
    | Parallel

  type eacsl_statistics =
    {
      satisfied: int;
      total: int;
      errors: int;
    }

end
include TYPES

let config_section =
  let default =
    {
      eacsl_enabled = false;
      eacsl_mode = Sequential;
      eacsl_gcc_path = "e-acsl-gcc.sh";
    }
  in
  Sc_config.Section.define "eacsl" ~default ~entries:Sc_config.Eztoml.[
      bool                                                        (* runtime? *)
        ~key:"enable"
        ~doc:"Enables post-processing with E-ACSL (default: %a)"
        ~default:default.eacsl_enabled
        ~as_flag:(Positive { keys = `Alt ["eacsl"; "e-acsl"; "E-ACSL"];
                             doc = `Same})
        (fun c b -> { c with eacsl_enabled = b })
        (fun c -> c.eacsl_enabled);
      string
        ~key:"mode"
        ~doc:"E-ACSL post-processing mode. Available modes are: parallel \
              (activated with \"p\"), and sequential (activated with \"s\"). The \
              parallel mode will switch to sequential if it fails too many \
              times.  Defaults to %a."
        ~default:"sequential"
        (fun c mode  ->
           match String.lowercase_ascii mode with
           | "sequential" | "seq" | "s" ->
               { c with eacsl_mode = Sequential }
           | "parallel" | "par" | "p" ->
               { c with eacsl_mode = Parallel }
           | _ ->
               raise @@ Sc_config.Errors.Invalid_string_value
                 { key = "mode"; value = mode;
                   descr = Some "E-ACSL post-processing mode" })
        (fun c -> match c.eacsl_mode with
           | Sequential -> "s"
           | Parallel -> "p");
      string'
        ~key:"path"
        ~doc:"Path to the e-acsl-gcc.sh executable"
        ~env:"EACSL_GCC"
        ~runtime:true
        ~default:default.eacsl_gcc_path
        (fun c s -> { c with eacsl_gcc_path = s })
        (fun c -> c.eacsl_gcc_path);
    ]

type eacsl_status =
  | SatisfiedACSL
  | UnsatisfiedACSL
  | Unknown
  | Error of exn

type eacsl_result = [`C] Sc_sys.File.t * eacsl_status

let make_eacsl_module config =
  Sc_project.Manager.make_ltest_module config
    ~workspace:(Sc_core.Workspace.mksub config.project_workspace "eacsl")

(* Returns the total of files whose acsl spec are satisfied and
   the total of files. *)
let eacsl_statistics: eacsl_result list -> eacsl_statistics =
  {satisfied = 0; total = 0; errors = 0} |>
  List.fold_left begin fun {satisfied = s; total = t; errors = e} ->
    function | _, SatisfiedACSL -> {satisfied = s + 1; total = t + 1; errors = e}
             | _, (UnsatisfiedACSL | Unknown) -> {satisfied = s; total = t + 1; errors = e}
             | _, Error _ -> {satisfied = s; total = t + 1; errors = e + 1}
  end

(* There are two execution modes of eacsl:
   - sequential;
   - parallel.
     In the first case, eacsl is called on each file separately and results are
     returned.
     In the second case, they are all called simultaneously, which often lead to
     compilation errors. When replaying a second time, two behaviours are
     possible:
   - replaying e-acsl in parallel works fine;
   - replaying e-acsl in parallel fails the exact same way on the same tests.
     Even if e-acsl fails in parallel, it always works fine sequentially (even on
     tests that have previously failed in parallel).
*)
let post_process_eacsl_loop ~(project: _ Sc_project.Types.project) ~options l =
  let module Ltest = (val make_eacsl_module project.config) in
  let parallel = options.eacsl_mode = Parallel in
  let rec loop run ~parallel l =
    (* parallel = true -> parallel mode *)
    (* parallel = false -> sequential mode *)
    let* () = Log.LWT.info "Run %i" run in
    let prev_stats = eacsl_statistics l in
    let* result =
      (if parallel then Lwt_list.map_p else Lwt_list.map_s)
        begin fun ((file, status) as elt) ->
          match status with
          | SatisfiedACSL | UnsatisfiedACSL ->
              Lwt.return elt
          | Unknown | Error _ ->
              Lwt.catch begin fun () ->
                let* is_ok =
                  Ltest.Eacsl.call_eacsl_on_file file
                    ~codebase:[project.codebase]
                    ~eacsl_gcc_path:options.eacsl_gcc_path
                in
                if is_ok
                then Lwt.return (file, SatisfiedACSL)
                else Lwt.return (file, UnsatisfiedACSL)
              end begin fun err ->
                Log.LWT.debug "Error while post-processing %a with e-acsl"
                  Sc_sys.File.print file >|= fun () ->
                file, Error err
              end
        end
        l
    in
    let new_stats = eacsl_statistics result in
    if run = 0 && List.length l = new_stats.errors then
      let* () =
        Log.LWT.err
          "Aborting e-acsl execution. It looks like e-acsl-gcc.sh is failing on \
           every test. You can re-try by replacing the e-acsl-gcc.sh script in \
           '%s/bin' by the one in '<seacoral-source-code>/scripts/'" @@
        Sc_sys.Env.get "OPAM_SWITCH_PREFIX" ~default:"$(OPAM_SWITCH_PREFIX)"
      in
      Lwt.return new_stats
    else if not parallel || new_stats.errors = 0
    then Lwt.return new_stats
    else begin
      Log.info "%i errors have been raised while starting e-acsl\
               " new_stats.errors;
      (* Retrying in parallel if the amount of errors has changed. *)
      let parallel = prev_stats.errors <> new_stats.errors in
      loop (run + 1) ~parallel result
    end
  in
  loop 0 ~parallel (List.map (fun t -> t, Unknown) l)

let run p testsuite =
  let options = Sc_config.Section.get config_section in
  if not options.eacsl_enabled
  then Lwt.return_none
  else
    let tests = match testsuite with
      | Split_testsuite { cov; _ } ->
          List.map (fun (t: individual_test_in_testsuite) -> t.file) cov
      | Combined_testsuite { cov; _ } ->
          [cov.file]
    in
    Lwt.return_some =<< post_process_eacsl_loop ~project:p ~options tests
