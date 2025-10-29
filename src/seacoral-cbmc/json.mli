(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Definition of the JSON encodings for parsing CBMS's json output. *)

open Types.DATA
open Json_encoding

(** Most of these encodings are exported for testing. *)
module Input : sig
  val options : json_options encoding
end

module Output : sig
  val source_location : source_location encoding
  val message : message encoding
  val program_info : string encoding
  val cell : 'a encoding -> 'a cell encoding
  val property : property encoding
  val properties : property list encoding
  val property_data : property list cell list encoding
  val base_value : base_value encoding
  val structure_field : structure_field encoding
  val structured : structured encoding
  val value : value encoding
  val input : input encoding
  val tests : test encoding
  val goal : goal encoding
  val goals_details : goals_details encoding
  val cbmc_cover_output : cbmc_cover_output encoding
  val cover_analysis_output : cbmc_cover_output cell list encoding
  val assert_analysis_result : cbmc_assert_output encoding
  val assert_analysis_output : cbmc_assert_output cell list encoding
  val assignment : assignment encoding
end

(** Returns the json representation of cbmc options. *)
val options : json_options -> string

(** [read_cbmc_output encoding json]

    Reads the result of CBMC. When CBMC returns an invalid json string,
    tries to fix it. *)
val read_cbmc_output : 'a encoding -> string -> 'a
