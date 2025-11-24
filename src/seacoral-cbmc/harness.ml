(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Harness generation *)

open Basics
open Sc_values.TYPES
open Sc_project.Types
open Sc_sys.File.Syntax
open Types
open Sc_C.Type_collections.ALIASES

module AP = Sc_C.Access_path

let log_src = Logs.Src.create ~doc:"Logs of Harness module" "Sc_cbmc.Harness"
module Log = (val (Ez_logs.from_src log_src))

let nondet_call ppf typ = Fmt.pf ppf "nondet_%s()" typ

let bool_of_value: DATA.effective_base_value -> string = function
  | { vbinary = Some ("00000000" | "00000001"); vdata; _ } -> vdata
  | { vbinary = Some binary; _ } -> "0b"^binary
  | { vdata; _ } -> vdata

let str_of_value (bv : DATA.effective_base_value) =
  (* Enums values may start with /*enum*/ *)
  match bv.vtype with
  | Some "_Bool" ->
      bool_of_value bv
  | Some "enum"
  | None when String.starts_with ~prefix:"/*enum*/" bv.vdata ->
      String.sub bv.vdata 8 (String.length bv.vdata - 8)
  | _ ->
      bv.vdata

(* Takes a [base_input], adds the fact that [id] is equal to [value].
   [id] must correspond to an actual variable. If it is just a suffix,
   raises [Invalid_argument]. *)
let to_literal_binding_map
    ?(empty_array_assignment = false)
    (base_literal_map : Sc_values.literal_binding StrMap.t)
    (id : AP.t)
    (effective_value : DATA.effective_base_value)
  : Sc_values.literal_binding StrMap.t =
  let var = AP.origin' id
  and path = match AP.suffix id with None -> [] | Some s -> NEL.to_list s in
  (* We use LBStr "" as an equivalent of "none", meaning that if a literal
     is equal to LBStr "", it has not been associated to anything yet. *)
  let none = Sc_values.LBStr "" in
  (* Folds in parallel on a literal and a path, and returns the literal
     associated to the path in which the tail value is equal to
     [effective_value.vdata].  Instead of using an option as a neutral element
     for the current literal, we will use [LBStr ""] for typing reasons. If the
     [curr_literal] is equal to [LBStr ""], it means the value has not been set
     yet.  *)
  let rec new_assoc curr_literal path =
    match curr_literal, path with
    | lit, [] when lit = none && empty_array_assignment ->
       Sc_values.LBRef (LBLst [])
    | lit, [] when lit = none ->
        Sc_values.LBStr (str_of_value effective_value)
    | lit, [] ->
       (* The end of the path is already bound to a value. *)
       Log.err "Trying to associate %a with %s, while it was already \
                associated to %a."
         AP.print id effective_value.vdata Sc_values.pp_literal_binding lit;
       invalid_arg "Sc_cbmc.Harness.to_literal_binding_map"
    | lit, Sc_C.Types.Field_access f :: tl when lit = none ->
       (* First field access, creating a Map binding *)
       LBMap (StrMap.singleton f (new_assoc none tl))
    | Sc_values.LBMap m, Sc_C.Types.Field_access f :: tl ->
       (* Field access on an already existing map *)
       let field_lit =
         match StrMap.find_opt f m with
         | None -> none
         | Some l -> l
       in
       LBMap (StrMap.add f (new_assoc field_lit tl) m)
    | lit, Sc_C.Types.Field_access f :: _ ->
       Log.err "Trying to associate %a with %s, while field %s is already \
                associated to %a."
         AP.print id effective_value.vdata f Sc_values.pp_literal_binding lit;
       invalid_arg "Sc_cbmc.Harness.to_literal_binding_map"
    | lit, Sc_C.Types.Index_access 0 :: tl when lit = none -> (* Possible raw pointer *)
       LBRef (new_assoc none tl)
    | lit, Sc_C.Types.Index_access o :: tl when lit = none ->
       let arr = Array.make (o + 1) none in (* The reason we use LBStr "" *)
       arr.(o) <- new_assoc none tl;
       LBArr arr
    | LBRef v, Sc_C.Types.Index_access o :: tl ->
       (* We thought it was a pointer, but it is a whole array *)
       let arr = Array.make (o + 1) none in
       if o = 0 then
         (* We are modifying the value v *)
         LBRef (new_assoc v tl)
       else begin
           (* We are modifying another cell of the array*)
           arr.(0) <- v;
           arr.(o) <- new_assoc none tl;
           LBArr arr
         end
    | LBArr arr, Sc_C.Types.Index_access o :: tl ->
       let new_arr =
         let arr_len = Array.length arr in
         if arr_len <= o then
           (* Extending arr *)
           Array.append arr (Array.make (o + 1 - arr_len) none)
         else arr
       in
       new_arr.(o) <- new_assoc new_arr.(o) tl;
       LBArr new_arr
    | lit, Sc_C.Types.Index_access o :: _ ->
       Log.err "Trying to associate %a with %s, while array at index %i already \
                holds the value %a."
         AP.print id effective_value.vdata o Sc_values.pp_literal_binding lit;
       invalid_arg "Sc_cbmc.Harness.to_literal_binding_map"
  in
  let current_var =
    match StrMap.find_opt var base_literal_map with
    | Some v -> v
    | None -> none
  in
  let v = new_assoc current_var path in
  StrMap.add var v base_literal_map

module TYPES = struct

  type support_data =
    {
      params: any_project_params;
      labels: Sc_ltest.Types.labels;
      pointer_memory_map : memory_validity AP.Map.t
    }

  type printer =
    {
      pp_label_decl: PPrt.pu;
      pp_body: (Format.formatter -> env);
    }

  and env =
    {
      entrypoint: string;
      mutable inputs: AP.t StrMap.t;  (* CBMC input argument => Access_path *)
      mutable empty: AP.t StrMap.t;   (* Pointer => non-NULL empty array flag *)
      mutable tmp_counter: int; (* A counter for temporary variables to enforce uniqueness *)
    }
end

include TYPES

type t = env

let empty_env entrypoint =
  {
    entrypoint;
    inputs = StrMap.empty;
    empty = StrMap.empty;
    tmp_counter = 0;
  }

let entrypoint t =
  t.entrypoint

(* --- *)

let pp_ctypes_static fmt t =
  let t = Ctype_key.of_typ t in
  Ctype_key.print fmt t

let support_data (type r) (project : r Sc_project.Types.project) =
  let params = project.params in
  let module Test_repr = (val params.test_repr) in
  (* Maps an access path to the memory *)
  let pointer_memory_map =
    Test_repr.fold_access_paths params.test_struct AP.Map.empty
      ~f_ptr:{
        f = fun _t ap_suffix pv map ->
          let ap = AP.HACK.forget_first_suffix_punct @@ Option.get ap_suffix in
          AP.Map.add ap pv map
      }
  in
  {
    params = A params;
    labels = project.labels;
    pointer_memory_map
  }

(* ---- *)

(** Prints a __CPROVER_input call for the variable specified by the Access_path
    [id] of type [t]. This function can be called for leaf types (i.e. non
    array, non pointer, non structure) *)
let make_symbolic_base ~env ppf (t, id) =
  let v = AP.to_string id in
  Log.debug "Symbolizing value %S" v;
  (* Checking if we did not already initialize it. *)
  if not @@ StrMap.mem v env.inputs then begin
    let c_ty = Fmt.str "%a" pp_ctypes_static t in
    let ty = String.replace_spaces ~by:'_' c_ty in
    let tmp = Format.sprintf "__tmp_%i" env.tmp_counter in
    env.tmp_counter <- env.tmp_counter + 1;
    env.inputs <- StrMap.add v id env.inputs;
    Fmt.pf
      ppf
      "%s %s = %a;@,\
       %s = %s;@,\
       __CPROVER_input(%S, %s);"
      c_ty tmp nondet_call ty
      v  tmp
      v tmp
  end

(** Symbolizes each of the [size] cells of the array specified by the
    Access_path [id]. *)
let make_symbolic_array
      ~env
      ~(make_symbolic: env:t -> Format.formatter -> AP.t -> unit ctyp_consumer)
      (ppf : Format.formatter)
      (size, id) : unit ctyp_consumer =
  {
    f = fun arr_typ ->
        for cpt = 0 to size - 1 do
          let cell_id = AP.append_index id cpt in
          (make_symbolic ~env ppf cell_id).f arr_typ
        done
  }

let pp_cstring_input ~env ~id ~buff_len ppf =
  let v = AP.to_string id in
  Fmt.pf ppf "@[<v 2>do {@;";
  Fmt.pf ppf "/* Initializing string '%s' */@," v;
  Fmt.pf ppf "static char __str[%u];@," (buff_len + 1); (* Note: hardcoded char *)
  for i = 0 to buff_len - 1 do
    let path = Fmt.str "%s[%u]" v i in
    Fmt.pf ppf "__CPROVER_input (%S, __str[%u]);@," path i;
    env.inputs <- StrMap.add path (AP.append_index id i) env.inputs;
  done;
  Fmt.pf ppf "__CPROVER_assume (__str[%u] == '\\000');@," buff_len;
  Fmt.pf ppf "%s = __str;" v;
  Fmt.pf ppf "@]@,} while (0);"

(** Prints a set of instruction for allocating a pointer [id] to a dynamic size,
    but with static_calls up to a given [max_size]. *)
let pp_static_malloc ~env ~id ~size_var ~max_size ~typ ppf =
  let typ_size = Ctypes_static.sizeof typ in
  let n = AP.to_string id in
  let empty_array_flag = Fmt.str "empty-array:%s" n in
  env.empty <- StrMap.add empty_array_flag id env.empty;
  Fmt.pf ppf "@[<v 2>do {@;";
  Fmt.pf ppf "/* Initializing pointer '%s' */@," n;
  Fmt.pf ppf "char __empty = %a;@," nondet_call "char";
  Fmt.pf ppf "__CPROVER_input (\"%s\", __empty);@," empty_array_flag;
  Fmt.pf ppf "if (__empty) {@,";
  Fmt.pf ppf "  static %a;@," Sc_values.Printer.c_decl (Array (typ, 0), "x");
  Fmt.pf ppf "  %s = x;@," n;
  Fmt.pf ppf "  __CPROVER_assume(%s == 0);@," size_var;
  Fmt.pf ppf "}@,";
  Fmt.pf ppf "else if (%s == 0) %s = 0;@," size_var n;
  for i = 0 to max_size do
    Fmt.pf ppf "else if (%s == %i) %s = malloc(%i);@,\
               " size_var i n (typ_size * i)
  done;
  Fmt.pf ppf "else __CPROVER_assume(0);";
  Fmt.pf ppf "@]@,} while (0);"

let fresh_size_var =
  let cpt = ref 0 in
  fun () ->
  let res = Fmt.str "size_%i" !cpt in
  incr cpt;
  res

let fresh_cases_var =
  let cpt = ref 0 in
  fun () ->
  let res = Fmt.str "cases_%i" !cpt in
  incr cpt;
  res

(** Returns a type consumer that prints the proper initialization of the
    value represented by the access path [id]. [id] must represent an actual
    input of the tested function. *)
let rec make_symbolic_cons
  ~sd:({ params = A params; _ } as sd)
  ~(env : t)
  (ppf : Format.formatter)
  (id : AP.t) : unit ctyp_consumer =
  let module Test_repr = (val params.test_repr) in
  let module Encoding = Sc_values.Encoding (Test_repr.Params) in
  { f = fun (type a) (t : a Ctypes_static.typ) ->
        match t with
      | Ctypes_static.Array (t, size) ->
         Log.debug "Symbolizing array %a" AP.print id;
         (make_symbolic_array
            ~env
            ~make_symbolic:(make_symbolic_cons ~sd)
            ppf
            (size, id)).f t
      | Ctypes_static.Pointer pted ->
         Log.debug "Symbolizing pointer %a" AP.print id;
         let pp_initialize_referenced_values ~id ~size_var ~max =
           Fmt.pf ppf "@[<v>/* Initializing referenced values */@,";
           for cpt = 0 to max - 1 do
             let new_id = AP.append_index id cpt in
             Fmt.pf ppf "@[<v 2>if (%s > %i) {@," size_var cpt;
             (make_symbolic_cons ~sd ~env ppf new_id).f pted;
             Fmt.pf ppf "@]@,}@,"
           done;
           Fmt.pf ppf "@]"
         in
         Encoding.ctyp_fold_direct_access_paths t () ~f_ptr:{
             f = fun _ ap_suffix _pv () ->
               let new_id =
                 match ap_suffix with
                 | None -> id
                 | Some suff -> AP.append id suff
               in
               match AP.Map.find new_id sd.pointer_memory_map with
               | exception Not_found ->
                  Log.debug
                    "Cannot find memory validity of %a, assuming too deep"
                    AP.print new_id;
                  Fmt.pf ppf "%s = 0;" (AP.to_string new_id);
               | `Carray_with_bound_length max ->
                  let size_var = fresh_size_var () in
                  Fmt.pf ppf "int %s = %a;@," size_var nondet_call "int";
                  pp_static_malloc ~env ~id:new_id ~size_var ~max_size:max ~typ:pted ppf;
                  Fmt.pf ppf "@,";
                  pp_initialize_referenced_values ~id:new_id ~size_var ~max
               | `Carray_with_length_field f ->
                  let size_ap = AP.HACK.forget_first_suffix_punct f.ap_suffix in
                  let size_var = AP.to_string size_ap in
                  let BoxedType t = Sc_values.struct_field_typ f.length_field in
                  (* Initializing the size variable if not already done *)
                  make_symbolic_base ~env ppf (t, size_ap);
                  Fmt.pf ppf "@,";
                  let max = Test_repr.Params.encoding_params.max_ptr_array_length in
                  pp_static_malloc ~env ~id:new_id ~size_var ~max_size:max ~typ:pted ppf;
                  Fmt.pf ppf "@,";
                  pp_initialize_referenced_values ~id:new_id ~size_var ~max
               | `Cstring ->
                  pp_cstring_input ~env ~id:new_id ppf
                    ~buff_len:Test_repr.Params.encoding_params.max_cstring_length;
                  Fmt.pf ppf "@,"
           }
      | Ctypes_static.Struct {fields; _} ->
         List.iter
           (fun (Ctypes_static.BoxedField {fname; ftype; _}) ->
             let id = AP.append_field id fname in
             (make_symbolic_cons ~sd ~env ppf id).f ftype)
           fields
      | Ctypes_static.Union {ufields = []; _} ->
         (* Is this even possible? *)
         make_symbolic_base ~env ppf (t, id)
      | Ctypes_static.Union {ufields; _} ->
         let case_var = fresh_cases_var () in
         Fmt.pf ppf "char %s = %a;@," case_var nondet_call "char";
         List.iteri
           (fun i (Ctypes_static.BoxedField {fname; ftype; _}) ->
             let id = AP.append_field id fname in
             Fmt.pf ppf "if (%s == %i) {@,  @[<v 2>" case_var i;
             (make_symbolic_cons ~sd ~env ppf id).f ftype;
             Fmt.pf ppf "@]@,} else "
           )
           ufields;
         (* Finishing with an else as there is at least one element in
            the list *)
         Fmt.pf ppf " __CPROVER_assume(0);"
      | t ->
         make_symbolic_base ~env ppf (t, id)
  }

(** Takes a variable name [v] of type [t], and prints its symbolization
    for the CBMC harness on [ppf]. *)
let make_symbolic ~sd ~env ppf (t, v) =
  Log.debug "Symbolizing %S" v;
  (make_symbolic_cons ~sd ~env ppf (AP.origin_only v)).f t

let symbolize_inputs env ppf ({ params = A params; _ } as sd) =
  let symbolize_var t v () =
    make_symbolic ~sd ~env ppf (t, v)
  in
  Sc_values.Struct.fold_fields params.test_struct ()
    ~f:{ f = symbolize_var }

let label_decl sd ppf =
  let lbls = sd.labels.simpl in
  List.iter
    (fun ((S l) : [`simple] Sc_C.Types.cov_label) ->
      Fmt.pf ppf "sc_cov_label_declare(%i);@," l.cov_label_id
    )
    lbls

let cover_labels ppf sd =
  let lbls = sd.labels.simpl in
  List.iter
    (fun ((S l) : [`simple] Sc_C.Types.cov_label) ->
      Fmt.pf ppf "sc_cover_statement(%i);@," l.cov_label_id
    )
    lbls

let declare_tested_function_args ppf { params = A params; _ } =
  (* Declare local variables for function arguments: we use the type of fields
     for this purpose so assignments are consistent w.r.t the inputs
     structure. *)
  let args = Sc_C.Defs.varset params.func_repr.func_args in
  let declare_arg typ v () =
    if Strings.mem v args
    then Fmt.pf ppf "%a;@;" Sc_values.Printer.c_decl (typ, v)
  in
  Sc_values.Struct.fold_fields params.test_struct ()
    ~f:{ f = declare_arg }

let emit_oracle_assessment_macro ppf =
  Fmt.pf ppf "#define __sc_assess_oracle(e)                   \\\
              @\n  __CPROVER_assert (e, \"oracle condition\")@\n"

let body ({ params = A params; _ } as sd) ppf =
  let env = empty_env "main" in
  Fmt.pf ppf
    "\
     %t\
     @\nint main () {\
     @\n  @[<v>%a@]\
     @\n  @[<v>%a@]\
     @\n  @[<v>%t@]\
     @\n  @[<v>%a@]\
     @\n  return 0;\
     @\n}
     @."
    emit_oracle_assessment_macro
    declare_tested_function_args sd
    (symbolize_inputs env) sd
    (Sc_project.Printer.C.emit_testcall params
       ~oracle_assessment:"__sc_assess_oracle"
       ~emit_effective_inputs:Sc_C.Printer.pp_vars params.func_repr.func_args)
    cover_labels sd;
  env

let printer sd =
  {
    pp_label_decl = label_decl sd;
    pp_body = body sd;
  }

let pp_include ppf filename =
  Fmt.pf ppf "#include %S@\n" filename

let generate ~project ~target ~cbmc_driver =
  let sd = support_data project in
  let {pp_body; pp_label_decl} = printer sd in
  let labelized_file = project.label_data.labelized_file in
  let>% ppf = target in
  Log.debug "Writing@ harness@ file@ `%a'" Sc_sys.File.print target;
  pp_include ppf (Sc_sys.File.absname cbmc_driver);
  pp_label_decl ppf;
  pp_include ppf (Sc_sys.File.absname labelized_file);
  pp_body ppf

(* --- *)

let apply_assignment_to_literal_map ~id ~value env map =
  match StrMap.find_opt id env.inputs with
  | Some id ->
     to_literal_binding_map map id value
  | None ->
      match StrMap.find_opt id env.empty with
      | Some id ->
          if value.vdata <> "0"
          then to_literal_binding_map ~empty_array_assignment:true map id value
          else map
      | None ->
          Log.debug "Unknown identifier %s, assuming internal" id;
          map

let test_to_literal env t =
  let map =
    List.fold_left
      (fun map DATA.{iid = id; ivalue} ->
          match ivalue with
          | Base Value value ->
            apply_assignment_to_literal_map ~id ~value env map
          | Base (Unknown _) -> map
          | _ -> assert false
      )
      StrMap.empty
      t.DATA.tinputs
  in
  let res = Sc_values.LBMap map in
  Log.debug "Computed value: %a" Sc_values.pp_literal_binding res;
  res

let is_cbmc_internal =
  let s = Str.regexp "^__CPROVER_" in
  fun name -> Str.string_match s name 0

let trace_to_literal (env : t) (trace : DATA.instruction list) =
  let rec loop (map : Sc_values.literal_binding StrMap.t) (l : DATA.instruction list) =
    match l with
    | [] -> Sc_values.LBMap map
    | (Function _ | Location _ | FailureStep _ | Assignment _ | Output _) :: tl ->
      loop map tl
    | Input { iinputID = i; _ } :: tl when is_cbmc_internal i ->
      loop map tl
    | Input { iinputID = id; ivalues = [Base Value value]; _ } :: tl ->
      loop (apply_assignment_to_literal_map ~id ~value env map) tl
    | Input { iinputID = id; ivalues = [_]; _ } :: _ ->
      Fmt.kstr failwith "Input %s with invalid value" id
    | Input { iinputID = id; ivalues = []; _ } :: _ ->
      Fmt.kstr failwith "Input %s with no value" id
    | Input { iinputID = id; ivalues = _ :: _; _} :: _ ->
      Fmt.kstr failwith "Forbidden input %s with a list of values" id
  in
  loop StrMap.empty trace
