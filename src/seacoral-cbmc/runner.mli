(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to run CBMC. *)

open Types

open Sc_sys.File.TYPES

type 'a process_result = 'a                                  (* always provided *)

type 'a cbmc_run =
  store:Sc_store.t ->
  runner_options:runner_options ->
  entrypoint:string ->
  files:[ `C ] Sc_sys.File.t list ->
  OPTIONS.t ->
  ('a Lwt_stream.t * (unit -> unit Lwt.t)) Lwt.t

(** Given a list of proof objectives and properties, returns the properties
    corresponding to unknown proof objectives.  They are matched with their
    location in the C file. *)
val uncovered_properties
  : mode: OPTIONS.mode
  -> harness_file:[`C] file
  -> labelized_file: [`C | `labelized] file
  -> cbmc_props:DATA.property list
  -> already_decided:Basics.Ints.t
  -> labels:Sc_C.Cov_label.simple list
  -> entrypoint:string
  -> [`simple] analysis_env

(** Starts a CBMC process that reads the file and return the list of properties *)
val cbmc_get_properties : DATA.property list DATA.cell cbmc_run

(** Starts a CBMC process that reads the file and return the list of cover
    objectives (i.e., instructions to cover) *)
val cbmc_get_cover_objectives : DATA.property list DATA.cell cbmc_run

(** Starts a CBMC process that reads the file and return the list of labels *)
val cbmc_get_clabels :
  lbls:(Sc_C.Cov_label.simple list)
  -> DATA.property list DATA.cell cbmc_run

(** Starts a CBMC process that analyzes the file and returns the test cases
    covering the labels. *)
val cbmc_cover_analysis :
  to_cover:[`simple] analysis_env
  -> DATA.cbmc_cover_output DATA.cell cbmc_run

(** Starts a CBMC process that checks if the labels are uncoverable by asserting
    the negation of their condition. *)
val cbmc_assert_analysis :
  to_cover:[`simple] analysis_env
  -> DATA.cbmc_assert_output DATA.cell cbmc_run

(** Same as [cbmc_assert_analysis], but uses the error labels feature of CBMC
    instead of assertions. *)
val cbmc_clabel_analysis :
  to_cover:[`simple] analysis_env
  -> DATA.cbmc_assert_output DATA.cell cbmc_run
