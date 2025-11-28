(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type coverable = [ `Cov of Sc_values.literal_binding * Basics.Ints.t ]

type res = [
    coverable
  | `Uncov of int
  | `NonValidExtra of string
  ]

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

(** [goal_stream_to_test_cases ~env ~harness ~stream kont]

    Reads the result of a CBMC cover analysis and calls [kont] on the
    result. *)
val goal_stream_to_test_cases :
  env:Types.simple_label_env
  -> harness:Harness.t
  -> stream:Types.DATA.cbmc_cover_output Types.DATA.cell Lwt_stream.t
  -> ((Sc_values.literal_binding * Basics.Ints.t) list -> unit Lwt.t)
  -> t Lwt.t

(** Same as [goal_stream_to_test_cases] for a CBMC assert/clabel analysis. *)
val assert_data_stream_to_test_cases :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  stream:Types.DATA.cbmc_assert_output Types.DATA.cell Lwt_stream.t ->
  ([ `Cov of (Sc_values.literal_binding * Basics.Ints.t)
   | `Uncov of int] -> unit Lwt.t) ->
  t Lwt.t

val goal_stream_to_test_cases_stream :
  env:Types.simple_label_env
  -> harness:Harness.t
  -> stream:Types.DATA.cbmc_cover_output Types.DATA.cell Lwt_stream.t
  -> res Lwt_stream.t

(** Same as [goal_stream_to_test_cases_stream] for a CBMC
    assert/clabel analysis. *)
val assert_data_stream_to_test_cases_stream :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  stream:Types.DATA.cbmc_assert_output Types.DATA.cell Lwt_stream.t ->
  res Lwt_stream.t 

(** Returns the data content of a list. *)
val only_data : 'a Types.DATA.cell list -> 'a list

val summing_up : res list -> t
