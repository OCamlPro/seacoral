(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Types

(** Printers *)

(** Pretty-prints coverage-related corpus info. *)
let pp_coverage_info ppf { num_tests_gen; num_tests_imported; _ } =
  let ifstl ~p ~s f ppf x = if p x then Fmt.styled s f ppf x else f ppf x in
  let tests = ifstl ~p:((=) 0) ~s:`Red @@ Fmt.styled `Bold @@ Fmt.uint in
  let num_tests = num_tests_gen + num_tests_imported in
  Fmt.fmt "%a test%s" ppf
    tests num_tests (if num_tests > 1 then "s" else "");
  if num_tests_imported > 0
  then Fmt.fmt " (%u imported)" ppf num_tests_imported

(** Pretty-prints RTE-related corpus info. *)
let pp_crash_info ppf { num_crash_gen; num_crash_imported; _ } =
  let ifstl ~p ~s f ppf x = if p x then Fmt.styled s f ppf x else f ppf x in
  let tests = ifstl ~p:((>) 0) ~s:`Red @@ Fmt.styled `Bold @@ Fmt.uint in
  let num_tests = num_crash_gen + num_crash_imported in
  Fmt.fmt "%a test%s" ppf
    tests num_tests (if num_tests > 1 then "s" else "");
  if num_crash_imported > 0
  then Fmt.fmt " (%u imported)" ppf num_crash_imported

(** Pretty-prints Oracle-related corpus info. *)
let pp_oracle_failures_info ppf { num_fails_gen; num_fails_imported; _ } =
  let ifstl ~p ~s f ppf x = if p x then Fmt.styled s f ppf x else f ppf x in
  let tests = ifstl ~p:((>) 0) ~s:`Red @@ Fmt.styled `Bold @@ Fmt.uint in
  let num_tests = num_fails_gen + num_fails_imported in
  Fmt.fmt "%a test%s" ppf
    tests num_tests (if num_tests > 1 then "s" else "");
  if num_fails_imported > 0
  then Fmt.fmt " (%u imported)" ppf num_fails_imported


(** {2 RTE "identifiers"} *)

let pp_sanitizer_error_summary ppf = function
  | Heap_buffer_overflow { pc } ->
      Fmt.pf ppf "heap-buffer-overflow at pc=0x%Lx" pc
  | Global_buffer_overflow { pc } ->
      Fmt.pf ppf "global-buffer-overflow at pc=0x%Lx" pc
  | Invalid_memory_address { pc } ->
      Fmt.pf ppf "invalid-memory-address at pc=0x%Lx" pc
  | Arithmetic_error { pc } ->
      Fmt.pf ppf "arithmetic error at pc=0x%Lx" pc

let pp_test_outcome ppf = function
  | Covering_label i ->
      Fmt.pf ppf "covering@ labels@ %a" Basics.Ints.print i
  | Triggering_RTE err ->
      Fmt.pf ppf "triggering@ %a" pp_sanitizer_error_summary err
  | Oracle_failure ->
      Fmt.pf ppf "oracle@ failure"

let pp_revalidation_result ppf o =
  Fmt.pf ppf "%a@ (%t)" pp_test_outcome o.test_outcome
    (if o.with_new_coverage
     then Fmt.fmt "with@ new@ coverage"
     else Fmt.fmt "without@ new@ coverage")

(* --- *)

let pp_internal_error ppf = function
  | Unexpected_filename f ->
      Fmt.pf ppf "unexpected filename `%s'" (Sc_sys.File.absname f)
  | Unexpected_outcome o ->
      Fmt.pf ppf "unexpected %a test outcome" pp_test_outcome o


;; Printexc.register_printer begin function
  | INTERNAL_ERROR e ->
      Some (Basics.PPrt.to_string "%a" pp_internal_error e)
  | _ ->
      None
end;;
