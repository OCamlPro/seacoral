(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** A module for handling files. When a file value is created, it is ensured
    to exist.
    When creating a file value, one must make sure the directory in which the
    file is created exists, otherwise this module will raise an exception. *)

module TYPES: sig

  (** The type of files. Having such a value ensures the file actually exists
      (unless it is removed by other means). *)
  type +_ file

  (** An alias for directories *)
  type dir = [`dir] file

  (** {2 Exceptions} *)

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

  (** Exception raised by {!err_if} if called with [dry=true].  This is used
      when some operation that should be performed to properly setup a project
      is prevented due to the tool running in ``dry'' mode. *)
  exception DRY_OPERATION_ERROR of string

end
include module type of TYPES
  with type 'a file = 'a TYPES.file
   and type dir = TYPES.dir

type 'a t = 'a file

(** {2 Basics} *)

(** Returns the file name given at creation. *)
val name : _ t -> string

(** Returns the base name of the file. *)
val basename: ?chop_extension:bool -> _ t -> string

(** Returns the absolute path of the file. *)
val absname : _ t -> string

(** Returns the directory name containing the file. *)
val dirname : _ t -> string

(** Prints a file (given) name *)
val print: Format.formatter -> _ t -> unit

(** Prints the basename of a file *)
val print_basename: Format.formatter -> _ t -> unit

(** Prints the absolute name of a file *)
val print_absname: Format.formatter -> _ t -> unit

val exists: _ t -> bool
val is_dir: _ t -> bool
val stat: _ t -> Unix.stats
val descriptor: _ t -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr

(** [check_suffix file suff] checks whether [suff] is a suffix for the basename
    of [file]. *)
val check_suffix: _ t -> string -> bool

val chop_suffix: _ t -> string -> string

(** Both [dir f] and [parent f] return the directory containing [f]. *)
val dir : _ t -> dir
val parent: _ t -> dir

(** {2 Sanity checks} *)

val fail_on_missing: _ t -> unit
val fail_on_missing_dir: dir -> unit
val fail_on_existing: _ t -> unit

(** [err_if ~dry format args...] fails and raises {!TYPES.DRY_OPERATION_ERROR}
    with a string built using [format] and [args...], or succeeds and returns
    false.

    [dry] is [false] by default. *)
val err_if: ?dry:bool -> ('a, Format.formatter, unit, bool) format4 -> 'a

(** {3 Helpers for sanity checks} *)

val missing: _ t -> exn
val unexpected: _ t -> exn
val unix_error: ctx: string -> abstract list -> Unix.error * string * string -> exn

(** {2 "Creation" functions}

    Not necessarily reflected on the filesystem: please use sanity checks when
    relevant. *)

(** [check_name filename] raises {!INVALID_FILENAME} if [filename] does not
    desribe a valid name for a file. *)
val check_name: string -> unit
val from_name: string -> _ t

(** Creation functions below return file representations, and may perform
    various checks.  The three first variants below do not actually perform any
    modification on the filesystem.

    - {v assume* v} variants only check for invalid file names;

    - {v existing* v} variants may additionally raise {!MISSING} if the given
      file does not exist upon call;

    - {v not* v} variants may additionally raise {!UNEXPECTED} if the given file
      exists upon call, and {!MISSING} if the parent directory does not exist;

    - {v create_empty* v} acts like equivalent {v not* v} variants, except that
      the file is actually created;

    - {v mkdir* v} variants creates a directory (recursively ascending to it
      parents if they do not exist --- equivalently to "[mkdir -p]").  May raise
      {!UNEXPECTED} when a file is encountered in place of a directory;
*)

val assume: string -> _ t                              (* no check *)
val assume_with_suffix: _ t -> string -> _ t            (* no check *)
val assume_in: dir: dir -> string -> _ t                (* no check *)
val assume_dir: string -> dir                          (* no check *)
val assume_dir_in: dir: dir -> string -> dir            (* no check *)
val existing: string -> _ t                            (* checks for existance *)
val existing_in: dir: dir -> string -> _ t              (* checks for existance *)
val existing_dir: string -> _ t                        (* checks for existance *)
val existing_dir_in: dir: dir -> string -> _ t          (* checks for existance *)
val not_: string -> _ t                                (* checks for absence *)
val not_in: dir: dir -> string -> _ t                   (* checks for absence *)
val create_empty: string -> _ t                        (* checks for absence *)
val create_empty_in: dir: dir -> string -> _ t          (* checks for absence *)
val mkdir: string -> dir
val mkdir_in: dir: dir -> string -> dir


module PRETTY: sig
  (** Pretty variants for the functions above (taking a format string followed
      by corresponding arguments, in place of a plain string). *)
  val assume:                      ('a, _ t) Basics.PPrt.func
  val assume_in:       dir: dir -> ('a, _ t) Basics.PPrt.func
  val assume_dir:                  ('a, dir) Basics.PPrt.func
  val assume_dir_in:   dir: dir -> ('a, dir) Basics.PPrt.func
  val existing:                    ('a, _ t) Basics.PPrt.func
  val existing_in:     dir: dir -> ('a, _ t) Basics.PPrt.func
  val existing_dir:                ('a, dir) Basics.PPrt.func
  val existing_dir_in: dir: dir -> ('a, dir) Basics.PPrt.func
  val not_:                        ('a, _ t) Basics.PPrt.func
  val not_in:          dir: dir -> ('a, _ t) Basics.PPrt.func
  val create_empty:                ('a, _ t) Basics.PPrt.func
  val create_empty_in: dir: dir -> ('a, _ t) Basics.PPrt.func
  val mkdir:                       ('a, dir) Basics.PPrt.func
  val mkdir_in:        dir: dir -> ('a, dir) Basics.PPrt.func
end


(** {2 "Touching"} (create if missing) *)

val touch: _ t -> unit
val touch_in: dir: dir -> string -> unit
val touch_dir: dir -> unit


(** {2 Channels} *)

(** [open_append file] returns an out_channel for appending on [file]. *)
val open_append: _ t -> out_channel

(** [safe_open_in file cont]
    Opens an in channel on `file` and applies `cont` on the channel.
    If any exception is raised out of `cont` or `cont` finishes its
    execution, the channel is closed.
*)
val safe_open_in : _ t -> (in_channel -> 'a) -> 'a

(** [safe_open_out file cont]
    Opens an out channel on `file` and applies `cont` on the channel.
    If any exception is raised out of `cont`, the channel is closed.
*)
val safe_open_out : _ t -> (out_channel -> 'a) -> 'a

(** [safe_open_append file cont] opens an out channel for appending on `file`
   and applies `cont` on the channel.  If any exception is raised out of `cont`,
   the channel is closed.  *)
val safe_open_append : _ t -> (out_channel -> 'a) -> 'a


(** {2 Removal} *)

(** Removes a file.
    If the file is a directory, raises:
    {ul
    {- [EPERM] on POSIX compliant system}
    {- [EISDIR] on Linux >= 2.1.132}
    {- [EACCESS] on Windows}} *)
(* TODO: unified exception *)
val unlink: _ t -> unit

(** [rename_away ~old_file ~new_basename] renames [old_file] by
    [path_to_old_file/new_basename].  Appends a time-dependent suffix to
    [new_basename] in order to make the new filename unique.

    Returns the name of the replacement file.
 *)
val rename_away: old_file: _ t -> new_basename: string -> _ t

(** Same as [rename_away], but writes over the new_basename file if it already
    exists. *)
val rename_replace: old_file: _ t -> new_basename: string -> _ t

(** {2 Linking and copying} *)

(** [copy_in ~dir file] Creates a copy of [file] in [dir].  Replaces any
    existing file from [dir] that has the same basename as [file]. *)
val copy_in: dir:dir -> 'a t -> 'a t
val copy_channels: in_channel -> out_channel -> unit
val link: ?dry:bool -> 'a t -> 'b t -> unit
val link_as: ?dry:bool -> 'a t -> string -> 'a t
val link_in_dir: ?dry:bool -> dir:dir -> 'a t -> 'a t

(** {2 Searching among files or directories} *)

(** [find_until_root_from ~dir ~f] returns searches in the sequence of
    successive parents of [dir] (including [dir] itself) for a directory [d] for
    which a call [f d] does not raise [Not_found].  Once such a [d] is found,
    the result is returned. If the root of all directories is reached,
    [Not_found] is thrown. *)
val find_until_root_from: dir: dir -> f:(dir -> 'a) -> 'a

(** {2 Direct string contents manipulation} *)

val read: _ file -> string

(** {2 Digest/hash file or directory contents} *)

(** [digest f] assumes [f] is a regular file *)
val digest: _ t -> Digest.t

val hash_files: _ t list -> string
val hash_dir: dir -> string


(** {2 Syntax extensions} *)

module Syntax : sig
  val ( let< ) : _ t -> (in_channel -> 'a) -> 'a
  val ( let> ) : _ t -> (out_channel -> 'a) -> 'a
  val ( let>% ) : _ t -> (Format.formatter -> 'a) -> 'a
  val ( let>> ) : _ t -> (out_channel -> 'a) -> 'a
  val ( let>>% ) : _ t -> (Format.formatter -> 'a) -> 'a

  (** [dir / filename] is equivalent to [assume_in ~dir filename] *)
  val ( / ) : dir -> string -> _ t
end
