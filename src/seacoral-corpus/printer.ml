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
  | Heap_buffer_overflow addr ->
      Fmt.pf ppf "heap-buffer-overflow at 0x%Lx" addr
  | Invalid_memory_address addr ->
      Fmt.pf ppf "invalid-memory-address at 0x%Lx" addr
  | Arithmetic_error addr ->
      Fmt.pf ppf "arithmetic error at 0x%Lx" addr

let pp_test_outcome ppf = function
  | Covering_label ->
      Fmt.pf ppf "covering@ labels"
  | Triggering_RTE err ->
      Fmt.pf ppf "triggering@ %a" pp_sanitizer_error_summary err
  | Oracle_failure ->
      Fmt.pf ppf "oracle@ failure"

(* --- *)

type 'raw_test delayed_test_view_emitter = 'raw_test test_view * Basics.PPrt.pu

let pp_raw_test (type raw_test) ~(corpus: raw_test Main.corpus) ppf raw_test =
  let module Raw_test = (val Main.test_repr corpus) in
  Raw_test.Val.print ppf raw_test

let delayed_raw_test_printer ~corpus raw_test : Basics.PPrt.pu =
  fun ppf -> pp_raw_test ~corpus ppf raw_test

let delayed_test_view_printer ?(sep: Basics.PPrt.ufmt = ":@;")
    (type raw_test) ~(corpus: raw_test Main.corpus) (test_view: _ test_view) =
  Lwt.bind (Lazy.force test_view.raw) @@ fun r ->
  let printer ppf =
    Fmt.pf ppf "Test@ %u@ (%s)%(%)%t" test_view.metadata.serialnum
      (Digest.to_hex test_view.metadata.id) sep
      (delayed_raw_test_printer ~corpus r)
  in
  Lwt.return (test_view, printer)

let pp_tests: _ Fmt.t =
  let compare_tests (t1, _) (t2, _) =
    Int.compare t1.metadata.serialnum t2.metadata.serialnum
  and test ppf (_, pp) = pp ppf in
  fun ppf ->
    Fmt.(using (List.sort compare_tests) @@
         list ~sep:cut @@ hovbox ~indent:2 test) ppf

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
