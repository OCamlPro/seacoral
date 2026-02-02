(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printers for public types of this library. *)

val pp_coverage_info: Types.info Fmt.t
val pp_crash_info: Types.info Fmt.t
val pp_oracle_failures_info: Types.info Fmt.t

val pp_sanitizer_error_summary: Types.sanitizer_error_summary Fmt.t
val pp_test_outcome: Types.test_outcome Fmt.t

(* --- *)

type 'raw_test delayed_test_view_emitter

val delayed_test_view_printer
  : ?sep:Basics.PPrt.ufmt
  -> corpus:'raw_test Main.corpus
  -> 'raw_test Types.test_view
  -> 'raw_test delayed_test_view_emitter Lwt.t

val pp_tests: _ delayed_test_view_emitter list Fmt.t
