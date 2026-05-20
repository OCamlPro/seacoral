(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type params =
  {
    test_struct: Sc_values.Struct.typ;
    run_num: int;
  }

(** module for test inputs representation *)
type 'raw_test repr =
  (module Sc_values.Struct.REPR with type Val.t = 'raw_test)

type 'raw_test decoder_params =
  {
    cil: Cil.file;             (** CIL representation for the whole C project *)
    test_repr: 'raw_test repr;
    test_struct: Sc_values.Struct.typ;
  }

type 'r validator_params =
  {
    cil: Cil.file;             (** CIL representation for the whole C project *)
    func_repr: Sc_C.Types.func_repr;        (** representation of the entrypoint
                                                function *)
    func_header: [`h] Sc_sys.File.file;    (** C header file with declaration of
                                               entrypoint and globals *)
    test_repr: 'r repr;              (** module for test inputs representation *)
    test_struct: Sc_values.Struct.typ;
    test_timeout: float option;                    (** test vaidation timeout *)
    init_func: Sc_C.Types.func_repr option; (** optional initialization
                                                function *)
    oracle_func: Sc_C.Types.func_repr option;    (** optional oracle function *)
    labelized_file: [`C | `labelized] Sc_sys.File.file; (** C file with the
                                                            extra labels. *)
    max_concurrent_validations: int;                       (** no max if null *)
    validator_verbosity: int;  (** when strictly positive, enable some logging
                                   from validation executables (only one level
                                   for now) *)
    external_libs : string list
  }

(** A type that enables tools to either retrieve an actual test inputs value
    (via [raw]), or to create a filesystem link with a file containing this
    value (via [link]). *)
type 'a test_view =
  {
    raw: 'a Lazy.t;
    link: 'x. 'x Sc_sys.File.t -> unit Lwt.t;
    metadata: test_metadata;
  }
and test_metadata =
  {
    serialnum: int;     (** Ordinal (starting from 1) incremented with every new
                            registration of a test into the corpus *)
    toolname: string;   (** Name of the tool that first transmitted the test *)
    creatime: float;    (** Registration time (using local timezone) *)
    crearun: int;       (** Run during which the test was created *)
    id: Digest.t;       (** Test digest (computed on internal representation) *)
    outcome: test_outcome;     (** Purpose of the test (covering labels, RTE) *)
  }

and test_outcome =
  | Covering_label of Basics.Ints.t (** The test does not raise an RTE and
                                        Covers labels *)
  | Triggering_RTE of sanitizer_error_summary   (** The test triggers an RTE *)
  | Oracle_failure                              (** The test fails the oracle *)

and sanitizer_error_summary =
  | Heap_buffer_overflow of { pc: int64 }
  | Global_buffer_overflow of { pc: int64 }
  | Invalid_memory_address of { pc: int64 }                           (* SEGV *)
  | Arithmetic_error of { pc: int64 }

(** Revalidation/replay *)

type revalidation_result =
  {
    test_outcome: test_outcome;     (** Outcome of the test *)
    with_new_coverage: bool;        (** Holds if the test covers a yet uncovered
                                        label *)
  }

(** Corpus info *)
type info =
  {
    num_tests_gen: int;    (** Total number of label-covering tests generated *)
    num_tests_imported: int;   (** Number of tests provided by the user and used
                                   to cover labels *)
    num_crash_gen: int;    (** Total number of RTE-triggering tests generated *)
    num_crash_imported: int; (** Number of user-provided RTE-triggering tests *)
    num_fails_gen: int;    (** Total number of generated tests that fail the
                               oracle *)
    num_fails_imported: int; (** Number of user-provided tests that fail the
                                 oracle *)
  }

(** Seeding info *)
type seeding_info =
  {
    num_imported: int;
    num_generated: int;
  }

(** {2 Errors} *)

type internal_error =
  | Unexpected_filename: _ Sc_sys.File.file -> internal_error
  | Unexpected_outcome of test_outcome

exception INTERNAL_ERROR of internal_error
