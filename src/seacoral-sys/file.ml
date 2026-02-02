(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

let ( / ) = Filename.concat                              (* only in this file *)


(* Types *)

module TYPES = struct

  type +'a file = {
    name: string;
    absname: string;
    basename: string;
    dirname: string;
  }

  type dir = [`dir] file

  (* Exceptions *)
  exception INVALID_FILENAME of string
  exception MISSING of { file: 'a. 'a file }
  exception UNEXPECTED of { file: 'a. 'a file }

  type unix_error =
    {
      context: string;               (** describes {e when} the error occured *)
      file_operands: abstract list;  (** files that were operated on *)
      code: Unix.error; func: string; arg: string;   (** as per {!Unix_error} *)
    }
  and abstract = A: _ file -> abstract

  exception UNIX_ERROR of unix_error

  exception DRY_OPERATION_ERROR of string

end
include TYPES

type 'a t = 'a file

let pp_unix_error ppf { code; func = _; arg = _; context; file_operands } =
  let msg = Unix.error_message code in
  match file_operands with
  | [] ->
      Fmt.pf ppf "Error@ while@ %a:@;%s" Fmt.text context msg
  | files ->
      Fmt.pf ppf "Error@ while@ %a@ %a:@;%s" Fmt.text context
        (Basics.PPrt.with_oxford_comma
           (fun ppf (A e) -> Fmt.fmt "`%s'" ppf e.name)) files msg


;; Printexc.register_printer begin function
  | INVALID_FILENAME path ->
      Some (Basics.PPrt.to_string "Invalid name for a file: `%s'" path)
  | MISSING { file } ->
      Some (Basics.PPrt.to_string "Missing file %s" file.absname)
  | UNEXPECTED { file } ->
      Some (Basics.PPrt.to_string "Unexpected file %s" file.absname)
  | UNIX_ERROR e ->
      Some (Basics.PPrt.to_string "%a" pp_unix_error e)
  | DRY_OPERATION_ERROR e ->
      Some (Basics.PPrt.to_string "Operation aborted in dry mode: %a\
                                  " Fmt.text e)
  | _ ->
      None
end

(* --- *)

(** [err_if ~dry format args...] fails and raises {!DRY_OPERATION_ERROR} with a
    string built using [format] and [args...], or succeeds and returns false.

    [dry] is [false] by default. *)
let err_if ?(dry = false) fmt =
  if dry
  then Format.kasprintf (fun e -> raise @@ DRY_OPERATION_ERROR e) fmt
  else Format.ikfprintf (fun _ -> false) Format.str_formatter fmt

(* --- *)

(* Various accessors and predicates *)

let name f =
  f.name

let absname f =
  f.absname

let basename ?(chop_extension = false) f =
  if chop_extension
  then Filename.chop_extension f.basename
  else f.basename

let dirname f =
  f.dirname

let print fmt f =
  Format.pp_print_string fmt (name f)

let print_basename fmt f =
  Format.pp_print_string fmt (basename f)

let print_absname fmt f =
  Format.pp_print_string fmt (absname f)

let exists { absname; _ } =
  Sys.file_exists absname

let is_dir { absname; _ } =
  Sys.file_exists absname &&                              (* not really atomic *)
  Sys.is_directory absname

let stat { absname; _ } =
  Unix.stat absname

let descriptor f =
  Unix.openfile (name f)

let check_suffix f suffix =
  Filename.check_suffix f.basename suffix

let chop_suffix f suffix =
  Filename.chop_suffix f.name suffix


(* Sanity checks *)

let missing, unexpected, unix_error =
  let copy file = { file with name = file.name } in
  (fun file -> MISSING { file = copy file }),
  (fun file -> UNEXPECTED { file = copy file }),
  (fun ~ctx:context file_operands (code, func, arg) ->
     UNIX_ERROR { code; func; arg; context; file_operands })

let fail_on_missing file =
  if not (exists file) then raise @@ missing file

let fail_on_missing_dir dir =
  if not (exists dir) then raise @@ missing dir;
  if not (is_dir dir) then raise @@ unexpected dir

let fail_on_existing file =
  if exists file then raise @@ unexpected file


(* "Creation" (not necessarily reflected on the file system) *)

let relative_to_absolute str =
  if Filename.is_relative str
  then Sys.getcwd () / str
  else str

let check_name filename =
  let forbidden_char = function
    | '&' | '|' | ';'
    | '\x0C' | '\n' | '\r' | '\t' -> true
    | _ -> false
  in
  String.iter
    (fun c -> if forbidden_char c then raise @@ INVALID_FILENAME filename) filename

let from_name name =
  check_name name;
  {
    name;
    absname = relative_to_absolute name;
    basename = Filename.basename name;
    dirname = Filename.dirname name;
  }

let dir f = from_name (dirname f)
let parent = dir                                           (* alias for `dir` *)

let assume = from_name                               (* alias for `from_name' *)

let assume_with_suffix f suffix =
  assume @@ name f ^ suffix

let existing filename =
  let file = from_name filename in
  fail_on_missing file;
  file

let existing_dir dirname =
  let dir = from_name dirname in
  fail_on_missing_dir dir;
  dir

let not_ filename =
  let file = from_name filename in
  fail_on_missing_dir (from_name @@ dirname file);
  fail_on_existing file;
  file

let assume_dir: string -> dir =
  assume

let assume_in ~dir basename =
  assume (dir.name / basename)

let assume_dir_in ~dir basename =
  assume_dir (dir.name / basename)

let existing_in ~dir basename =
  existing (dir.name / basename)

let existing_dir_in ~dir basename =
  existing_dir (dir.name / basename)

let not_in ~dir basename =
  not_ (dir.name / basename)

(* --- *)

let build_in_existing_dir name : 'a t =
  let file = from_name name in
  fail_on_missing_dir (from_name @@ dirname file);
  file

let empty file =
  fail_on_existing file;
  close_out @@ open_out file.absname

let create_empty filename =
  let file = build_in_existing_dir filename in
  empty file;
  file

let create_empty_in ~dir basename =
  create_empty (dir.name / basename)


let mkdir (name: string) : dir =
  check_name name;
  let rec create dir =
    let exists = Sys.file_exists dir in
    if exists then begin
      if not @@ Sys.is_directory dir
      then raise (unexpected @@ from_name dir)
    end else begin
      create @@ Filename.dirname dir;
      make dir;
      Log.debug "Created directory `%s'" dir
    end
  and make dir =
    try (* [dir] may have been created between [Sys.file_exists] and this call.
           So we need to catch and ignore EEXIST. *)
      Unix.mkdir dir 0o755
    with
    | Unix.Unix_error (EEXIST, _, _) ->
        ()
    | Unix.Unix_error (code, f, a) ->
        raise @@ unix_error ~ctx:"creating" [A (from_name dir)] (code, f, a)
  in
  create name;
  from_name name

let mkdir_in ~dir subdir : dir =
  check_name subdir;
  let dir = name dir / subdir in
  let exists = Sys.file_exists dir in      (* should check dir info too *)
  let () =
    if exists then begin
      if not @@ Sys.is_directory dir
      then raise (unexpected @@ from_name dir)
    end
    else begin
      Unix.mkdir dir 0o755;
      Log.debug "Created directory `%s'" dir;
    end
  in
  from_name dir

module PRETTY = struct
  let assume               fmt = Format.kasprintf (assume              ) fmt
  let assume_in       ~dir fmt = Format.kasprintf (assume_in       ~dir) fmt
  let assume_dir           fmt = Format.kasprintf (assume_dir          ) fmt
  let assume_dir_in   ~dir fmt = Format.kasprintf (assume_dir_in   ~dir) fmt
  let existing             fmt = Format.kasprintf (existing            ) fmt
  let existing_in     ~dir fmt = Format.kasprintf (existing_in     ~dir) fmt
  let existing_dir         fmt = Format.kasprintf (existing_dir        ) fmt
  let existing_dir_in ~dir fmt = Format.kasprintf (existing_dir_in ~dir) fmt
  let not_                 fmt = Format.kasprintf (not_                ) fmt
  let not_in          ~dir fmt = Format.kasprintf (not_in          ~dir) fmt
  let create_empty         fmt = Format.kasprintf (create_empty        ) fmt
  let create_empty_in ~dir fmt = Format.kasprintf (create_empty_in ~dir) fmt
  let mkdir                fmt = Format.kasprintf (mkdir               ) fmt
  let mkdir_in        ~dir fmt = Format.kasprintf (mkdir_in        ~dir) fmt
end


let touch file =
  if not (exists file) then try empty file with UNEXPECTED _ -> ()

let touch_in ~dir basename =
  touch @@ build_in_existing_dir (dir.name / basename)

let touch_dir dir =
  ignore @@ mkdir dir.name

(* Removal *)

let unlink file =
  let name = name file in
  try
    Unix.unlink name;
    Log.debug "Removed@ file@ `%s'" name
  with Unix.Unix_error (code, f, a) ->
    raise @@ unix_error ~ctx:"removing" [A file] (code, f, a)

let rename_away ~old_file ~new_basename =
  (* Always add a timestamp to avoid collision *)
  let basename =
    Format.asprintf "%s-%a" new_basename
      Basics.PPrt.pp_time Unix.(gettimeofday () |> localtime)
  in
  let new_file = from_name (dirname old_file / basename) in
  try
    Sys.rename old_file.absname new_file.absname;
    new_file
  with Unix.Unix_error (code, f, a) ->
    raise @@ unix_error ~ctx:"renaming" [A old_file] (code, f, a)

let rename_replace ~old_file ~new_basename =
  let new_file = from_name (dirname old_file / new_basename) in
  try
    Sys.rename old_file.absname new_file.absname;
    new_file
  with Unix.Unix_error (code, f, a) ->
    raise @@ unix_error ~ctx:"renaming" [A old_file] (code, f, a)

(* Opening *)

let safe_open
    (open_file : string -> 'a)
    (close_chan : 'a -> unit)
    (file : _ t)
    (cont : 'a -> 'b) : 'b =
  let fname = absname file in
  let chan = open_file fname in
  try
    let res = cont chan in
    close_chan chan;
    res
  with
  | Unix.Unix_error (code, f, a) ->
      close_chan chan;
      raise @@ unix_error ~ctx:"opening" [A file] (code, f, a)
  | e ->
      close_chan chan;
      (* Don't log here as it's not typically an error in "openning" *)
      (* Log.err "Open %s failed: %a" fname Fmt.exn e; *)
      Lwt.reraise e                                   (* lwt reraise, in case *)

let safe_open_in
    (f : _ t)
    (cont : in_channel -> 'a) : 'a =
  safe_open open_in close_in f cont

let safe_open_out
     (f : _ t) (cont : out_channel -> 'a) : 'a =
  safe_open open_out close_out f cont

let raw_open_append =
  open_out_gen [Open_append; Open_creat] 0o644

let open_append f =
  raw_open_append (absname f)

let safe_open_append
     (f : _ t) (cont : out_channel -> 'a) : 'a =
  safe_open raw_open_append close_out f cont

module Syntax = struct
  let (let<) = safe_open_in
  let (let>) = safe_open_out
  let (let>%) file f =
    (let>) file (fun oc -> f @@ Format.formatter_of_out_channel oc)
  let (let>>) = safe_open_append
  let (let>>%) file f =
    (let>>) file (fun oc -> f @@ Format.formatter_of_out_channel oc)
  let (/) dir file = assume_in ~dir file
end

(* --- *)

let copy_channels ic oc =
  Stdlib.Out_channel.output_string oc @@
  Stdlib.In_channel.input_all ic

let copy_in ~dir (file : 'a t) : 'a t =
  let open Syntax in
  let file_res = assume_in ~dir (basename file) in
  let< old = file in
  let> new_ = file_res in                                  (* erase if exists *)
  copy_channels old new_;
  file_res

let link ?dry src dst =
  if not (exists dst) &&
     not (err_if ?dry "Missing link `%a'" print dst)
  then begin
    (* TODO: change directory to make a symbolic link instead? *)
    Log.debug "link: `%s' -> `%s'" (absname src) (absname dst);
    Unix.link (absname src) (absname dst)
  end

let link_as ?dry (src: 'a t) dst : 'a t =
  let res = assume dst in
  link ?dry src res;
  res

let link_in_dir ?dry ~(dir:dir) (src : 'a t) : 'a t =
  link_as ?dry src (name dir / basename src)

(* Searching *)

let find_until_root_from ~(dir: dir) ~f =
  let rec try_dir dir =
    try f dir with Not_found ->
      let new_dir = parent dir in
      if new_dir = dir
      then raise Not_found                                  (* we are at root *)
      else try_dir new_dir
  in
  try_dir dir

(* String-based manipulations *)

let read f =
  let open Syntax in
  let< ic = f in
  Stdlib.In_channel.input_all ic

(* Digest / Hashing *)

let digest f =
  Digest.file (absname f)

let hash_files files =
  let buff = Buffer.create 1000 in
  List.iter begin fun f ->
    try Buffer.add_string buff (digest f)
    with Sys_error _ -> ()                    (* ignore disapearing dirs/files *)
  end files;
  Buffer.contents buff

let hash_dir dir =
  let buff = Buffer.create 1000 in
  let rec aux f =
    try
      if Sys.is_directory f
      then Array.iter (fun g -> aux (Filename.concat f g)) (Sys.readdir f)
      else Buffer.add_string buff (Digest.file f)
    with Sys_error _ -> ()                    (* ignore disapearing dirs/files *)
  in
  if not (is_dir dir)
  then Format.kasprintf failwith "Directory %a not found" print dir
  else (aux (absname dir); Buffer.contents buff)
