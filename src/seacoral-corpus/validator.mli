(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type 'raw_test t
type 'raw_test ready
type validation_purpose =
  | For_RTE_identification
  | For_full_validation

val make
  : workspace: Sc_core.Types.workspace
  -> 'raw_test Decoder.t
  -> Sc_store.t
  -> 'raw_test Types.validator_params
  -> 'raw_test t Lwt.t

val setup
  : 'raw_test t
  -> 'raw_test ready Lwt.t

val validate_raw_test_file
  : _ ready
  -> ?purpose: validation_purpose
  -> _ Sc_sys.File.t
  -> Types.test_outcome option Lwt.t

(** Take care to avoid large amounts of concurrent calls to this function, as
    this may induce an overuse of system pipes.  In these cases, prefer using
    {!validate_raw_test_file} when possible. *)
val validate_raw_test_string
  : _ ready
  -> ?purpose: validation_purpose
  -> string
  -> Types.test_outcome option Lwt.t

(** Warning for {!validate_raw_test_string} applies. *)
val validate_raw_test
  : 'raw_test ready
  -> ?purpose: validation_purpose
  -> 'raw_test
  -> Types.test_outcome option Lwt.t

(** Warning for {!validate_raw_test_string} applies. *)
val validate_n_share_raw_test
  : 'raw_test ready
  -> corpus: 'raw_test Main.corpus
  -> toolname: string
  -> ?purpose: validation_purpose
  -> ?log_outcome: bool
  -> 'raw_test
  -> unit Lwt.t

(** Warning for {!validate_raw_test_string} does NOT apply. *)
val validate_n_share_raw_test_file
  : 'raw_test ready
  -> corpus: 'raw_test Main.corpus
  -> toolname: string
  -> ?purpose: validation_purpose
  -> ?log_outcome: bool
  -> _ Sc_sys.File.t
  -> unit Lwt.t
