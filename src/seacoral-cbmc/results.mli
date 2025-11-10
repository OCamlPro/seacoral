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

val goal_stream_to_test_cases :
  env:Types.simple_label_env
  -> harness:Harness.t
  -> stream:Types.DATA.cbmc_cover_output Types.DATA.cell Lwt_stream.t
  -> ((Sc_values.literal_binding * Basics.Ints.t) list -> unit Lwt.t)
  -> t Lwt.t

val assert_data_stream_to_test_cases :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  stream:Types.DATA.cbmc_assert_output Types.DATA.cell Lwt_stream.t ->
  ([ `Cov of (Sc_values.literal_binding * Basics.Ints.t)
   | `Uncov of int] -> unit Lwt.t) ->
  t Lwt.t


(** Returns the data content of a list. *)
val only_data : 'a Types.DATA.cell list -> 'a list
