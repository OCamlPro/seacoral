(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

(** [with_bidirectional_channel ?import_suff ?import_filter ?validation_purpose
    ?read_test ?write_test ~toolname corpus validator dirname f] calls [f] after
    having setup a bi-directional exchange channel with [corpus].

    According to the underlying exchange protocol, each file newly created in
    directory [dirname] and whose name does not terminate with [import_suff]
    ([".imported"] by default), is submitted to the validator [validator] after
    having been read using [read_test] (which is {!Main.read_raw_test} by
    default). Submission is performed in the name of [toolname], with purpose
    [purpose] ({!Validator.For_full_validation} by default).

    Reciprocally, the file of any test that: (i) is shared with [corpus] during
    the execution of [f] and until [with_bidirectional_channel] terminates, and
    (ii) whose metadata passes [import_filter] (if provided), is placed into
    [dirname] by using [write_test]: the latter is [`Link] by default, which
    means a filesystem link is used; alternatively a user-defined function
    [write] may be specified with [`Func write].  Files imported in this way are
    not subject to the aforementioned export mechanism. *)
val with_bidirectional_channel
  : ?import_suff: string
  -> ?import_filter: (Types.test_metadata -> bool Lwt.t)
  -> ?validation_purpose: Validator.validation_purpose
  -> ?read_test: (_ file -> 'raw_test Lwt.t)
  -> ?write_test: [< `Func of _ file -> 'raw_test -> unit Lwt.t
                  | `Link > `Link ]
  -> toolname: string
  -> 'raw_test Main.corpus
  -> 'raw_test Validator.ready
  -> dir
  -> (unit -> 'a Lwt.t)
  -> 'a Lwt.t

(** [import_tests ?import_suff ?write_test ?filter corpus dirname] imports every
    input previously (given to/shared with) [corpus] into the directory
    [dirname].  Inputs are imported locally using [write] if [write_test = `Func
    write], or using a file-system link otherwise (i.e, [write_test = `Link],
    the default).

    [filter] may be used to select tests to import based on their metadata ({i
    cf} {!Types.test_metatada}).

    The directory [dirname] must exist when this function is called.  *)
val import_tests
  : ?import_suff:string
  -> ?write_test:[< `Func of _ file -> 'raw_test -> unit Lwt.t
                 | `Link > `Link ]
  -> ?filter: (Types.test_metadata -> bool)
  -> 'raw_test Main.corpus
  -> dir
  -> Basics.Digests.t Lwt.t
