(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Abstract representation of CBMC results. *)

type t

val empty : t

(** Returns the tests registered. *)
val get_tests :
  t -> (Sc_values.literal_binding * Basics.Ints.t) list

(** Returns the covered labels. *)
val get_covered : t -> Basics.Ints.t

(** Returns the uncoverable labels. If there are non valid extra properties,
    returns the empty set. *)
val get_uncoverable : t -> Basics.Ints.t

(** Reads the result of a CBMC cover analysis. *)
val goals_to_test_cases :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  Types.DATA.cbmc_cover_output Types.DATA.cell list ->
  t

val goal_stream_to_test_cases :
  env:Types.simple_label_env
  -> harness:Harness.t
  -> stream:Types.DATA.cbmc_cover_output Types.DATA.cell Lwt_stream.t
  -> ((Sc_values.literal_binding * Basics.Ints.t) list -> unit Lwt.t)
  -> unit Lwt.t

(** Reans the result of a CBMC assert/clabel analysis. *)
val assert_data_list_to_test_cases :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  Types.DATA.assertion_check list Types.DATA.cell list ->
  t

val assert_data_stream_to_test_cases :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  stream:Types.DATA.cbmc_assert_output Types.DATA.cell Lwt_stream.t ->
  ([ `Cov of (Sc_values.literal_binding * Basics.Ints.t)
   | `Uncov of int] -> unit Lwt.t) ->
  unit Lwt.t


(** Returns the data content of a list. *)
val only_data : 'a Types.DATA.cell list -> 'a list
