(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Text I/O primitives *)

open Types

(** {2 RTE "identifiers"} *)

let print_sanitizer_error_summary = function
  | Heap_buffer_overflow { pc } ->
      Basics.PPrt.asprintf "rte:heap-buffer-overflow\t0x%Lx" pc
  | Global_buffer_overflow { pc } ->
      Basics.PPrt.asprintf "rte:global-buffer-overflow\t0x%Lx" pc
  | Invalid_memory_address { pc } ->
      Basics.PPrt.asprintf "rte:invalid-memory-address\t0x%Lx" pc
  | Arithmetic_error { pc } ->
      Basics.PPrt.asprintf "rte:arithmetic-error\t0x%Lx" pc

let print_cover_summary ints =
  Basics.PPrt.asprintf
    "cover\t%a"
    (fun fmt l -> List.iter (fun i -> Fmt.int fmt i; Fmt.string fmt "\t") l)
    (Basics.Ints.elements ints)

let print_oracle_fail () = "oracle-fail\t"

let print_summary () = function
  | Triggering_RTE err -> print_sanitizer_error_summary err
  | Covering_label i -> print_cover_summary i
  | Oracle_failure -> print_oracle_fail ()

let scan_list ic =
  let rec loop acc =
    try
      Scanf.bscanf ic "%i\t" begin fun i ->
        loop (Basics.Ints.add i acc)
        end
    with
    | End_of_file -> acc
  in
  loop Basics.Ints.empty

let scan_summary (ic: Scanf.Scanning.in_channel) =
  let rte err = Triggering_RTE err in
  Scanf.bscanf ic "%s@\t" begin function
    | "rte:heap-buffer-overflow" ->
        Scanf.bscanf ic "0x%Lx" (fun pc -> rte @@ Heap_buffer_overflow { pc })
    | "rte:global-buffer-overflow" ->
        Scanf.bscanf ic "0x%Lx" (fun pc -> rte @@ Global_buffer_overflow { pc })
    | "rte:invalid-memory-address" ->
        Scanf.bscanf ic "0x%Lx" (fun pc -> rte @@ Invalid_memory_address { pc })
    | "rte:arithmetic-error" ->
        Scanf.bscanf ic "0x%Lx" (fun pc -> rte @@ Arithmetic_error { pc })
    | "cover" ->
        Covering_label (scan_list ic)
    | "oracle-fail" ->
        Oracle_failure
    | key ->
        Fmt.kstr (fun s -> raise (Scanf.Scan_failure s))
          "unknown sanitizer error key %S" key
  end
