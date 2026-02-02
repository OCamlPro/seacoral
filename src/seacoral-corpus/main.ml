(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Basics
open Sc_core.Types
open Sc_sys.File.TYPES
open Types

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

(* --- *)

(* module TestsCache = Ephemeron.K1.Make (String) *)
module Tests_table = Hashtbl.Make (Digest)

module Crash_ident = struct
  type t = sanitizer_error_summary
  let equal = (=)
  let hash = Hashtbl.hash
end
module Crash_table = Hashtbl.Make (Crash_ident)

type 'r corpus =
  {
    tests_dir: dir;
    tests_mbox: (Digest.t * 'r given_test) option Lwt_mvar.t;
    tests_stream: (Digest.t * 'r given_test) Lwt_stream.t;
    tests_cache: 'r registered_test Tests_table.t;
    tests_cache_mutex: Lwt_mutex.t;
    crashes: Digest.t Crash_table.t;
    crashes': Crash_ident.t Tests_table.t;
    crashes_file: [`text] file;
    bypassed_count: int ref;
    bypassed_count_mutex: Lwt_mutex.t;
    bypassed_file: [`bin] file;
    params: 'r corpus_params;
  }
and 'r registered_test =
  {
    file: 'r Sc_sys.File.t;
    raw: 'r Lwt.t Lazy.t;
  }
and 'r given_test =
  {
    toolname: string;
    outcome: test_outcome;
    v: 'r;
  }

(* --- *)

let add_crash_entry { crashes_file; crashes; crashes'; _ } id err =
  let* () =
    let>>* oc = crashes_file in
    Lwt_io.fprintf oc "%s\t%a\n%!" (Digest.to_hex id)
      IO.print_sanitizer_error_summary err
  in
  Crash_table.add crashes err id;
  Tests_table.add crashes' id err;
  Lwt.return ()

let load_known_crash_table { crashes_file; crashes; crashes'; _ } =
  let<* ic = crashes_file in
  Lwt_io.read_lines ic |>
  Lwt_stream.iter begin fun line ->
    try
      Scanf.sscanf line "%[0-9a-fA-F]\t%r" IO.scan_sanitizer_error_summary
        begin fun id_hex err ->
          let id = Digest.from_hex id_hex in
          Crash_table.add crashes err id;
          Tests_table.add crashes' id err
        end
    with End_of_file | Scanf.Scan_failure _ -> ()
  end

(* --- *)

let internal_error e =
  raise @@ INTERNAL_ERROR e

let pp_outcome ppf = function
  | Covering_label -> Fmt.string ppf "cover"
  | Triggering_RTE _ -> Fmt.string ppf "rte"
  | Oracle_failure -> Fmt.string ppf "fail"

(* --- *)

let read_raw_test (type r) (corpus: r corpus) file =
  let module Raw_test = (val corpus.params.test_repr) in
  Raw_test.Val.of_string corpus.params.test_struct =|<
  Sc_sys.Lwt_file.read file

let read_test corpus f =
  Lwt.catch begin fun () ->
    Lwt.return_some =<< read_raw_test corpus f
  end begin function
    | Sc_values.Struct.Invalid_value_representation _ as e ->
        Log.warn "invalid input in `%a'; discarding" Sc_sys.File.print f;
        Log.debug "error: %s" (Printexc.to_string e);
        Sc_sys.File.unlink f;
        Lwt.return_none
    | Sys_error m ->
        (* Filter out some errors; may happen when a tool shares a corpus
           directory where files may appear only temporarily and are removed
           afterwards. *)
        Log.warn "%s" m;
        Lwt.return_none
    | e ->
        Lwt.reraise e
  end

let write_test (type r) (corpus: r corpus) f v =
  let module Raw_test = (val corpus.params.test_repr) in
  Sc_sys.Lwt_file.write f @@ Raw_test.Val.to_string v

let format_file run_num serialnum toolname id outcome =
  Fmt.str "%04u-@%u-%s-%s-%a"
    serialnum run_num (Digest.to_hex id) toolname pp_outcome outcome

let test_outcome_from_test_suffix { crashes'; _ } id = function
  | "rte" ->
      Ok (Triggering_RTE (Tests_table.find crashes' id))
  | "cover" ->
      Ok (Covering_label)
  | "fail" ->
      Ok (Oracle_failure)
  | _ ->
      Error ()

let test_outcome corpus f =
  try
    Scanf.sscanf (Sc_sys.File.basename f) "%_u-%@%_u-%s@-%_s@-%s" @@ fun id_hex ->
    test_outcome_from_test_suffix corpus @@ Digest.from_hex id_hex
  with Scanf.Scan_failure _ | Failure _ | End_of_file ->
    Error ()

let test_metadata corpus f =
  let* stat = Lwt_unix.stat (Sc_sys.File.name f) in
  try
    Lwt.return @@
    Scanf.sscanf (Sc_sys.File.basename f) "%u-%@%u-%s@-%s@-%s"
      begin fun serialnum crearun id_hex toolname effect_suffix ->
        let id = Digest.from_hex id_hex in
        let outcome =
          match test_outcome_from_test_suffix corpus id effect_suffix with
          | Ok outcome ->
              outcome
          | Error () ->
              internal_error @@ Unexpected_filename f        (* TODO: silent? *)
        in
        { serialnum; id; toolname; creatime = stat.st_mtime; crearun; outcome; }
      end
  with Scanf.Scan_failure _ | Failure _ | End_of_file ->
    internal_error @@ Unexpected_filename f                  (* TODO: silent? *)

let existing_test_files { tests_dir; _ } =
  Sc_sys.Lwt_file.files_of_dir tests_dir

let existing_test_ids ?(exclude = Digests.empty) { tests_cache;
                                                   tests_cache_mutex; _ } =
  Lwt_mutex.with_lock tests_cache_mutex begin fun () ->
    Digests.empty |>
    (Tests_table.to_seq tests_cache |>
     Lwt_stream.of_seq |>
     Lwt_stream.fold begin function
       | id, _ when Digests.mem id exclude -> fun m -> m
       | id, _ -> Digests.add id
     end)
  end

let existing_tests ?(exclude = Digests.empty) ({ tests_cache;
                                                 tests_cache_mutex;
                                                 _ } as corpus) =
  Lwt_mutex.with_lock tests_cache_mutex begin fun () ->
    Tests_table.to_seq tests_cache |>
    Lwt_stream.of_seq |>
    Lwt_stream.filter_map_s begin function
      | id, _ when Digests.mem id exclude ->
          Lwt.return None
      | _, { file; raw } ->
          let* metadata = test_metadata corpus file in
          let link out = Sc_sys.Lwt_file.link file out in
          Lwt.return (Some { raw; link; metadata })
    end |>
    Lwt.return
  end |>
  Lwt_stream.return_lwt |>
  Lwt_stream.concat

(** private *)
let cache_existing_tests ({ tests_cache; tests_cache_mutex; _ } as corpus) =
  existing_test_files corpus |>
  Lwt_stream.iter_s begin fun (f : _ Sc_sys.File.t) ->
    let* { serialnum = _; toolname = _; id; _ } = test_metadata corpus f in
    Log.info "Registering known input %s" (Digest.to_hex id);
    Log.debug "Corresponding file is %s" @@ Sc_sys.File.absname f;
    Lwt_mutex.with_lock tests_cache_mutex begin fun () ->
      Tests_table.add tests_cache id {
        file = f;
        raw = Lazy.from_fun @@ fun () -> read_raw_test corpus f;
      };
      Lwt.return ()
    end
  end

(** "private" task: receives tests coming from [share_test] *)
let receive_new_tests ({ tests_dir; tests_stream;
                         tests_cache; tests_cache_mutex;
                         crashes; params; _ } as corpus) =
  Lwt_stream.iter_s begin fun (id, { v; toolname; outcome }) ->
    let id_hex = Digest.to_hex id in
    Lwt_mutex.with_lock tests_cache_mutex begin fun () ->
      if Tests_table.mem tests_cache id then begin
        Log.LWT.debug "Input@ %s@ already@ known" id_hex
      end else begin match outcome with
        | Triggering_RTE err when Crash_table.mem crashes err ->
            Log.LWT.debug "Input@ %s@ triggers@ an@ already@ known@ crash:@ \
                           discarding" id_hex
        | _ ->
            let verdict = match outcome with
              | Covering_label -> "ok"
              | Triggering_RTE _ -> "rte"
              | Oracle_failure -> "fail"
            in
            Log.LWT.info "Saving@ new@ input@ %s@ (%s)\
                         " id_hex verdict >>= fun () ->
            let serialnum = Tests_table.length tests_cache + 1 in
            let basename =
              format_file params.run_num serialnum toolname id outcome
            in
            let file = tests_dir / basename in
            let* () = match outcome with
              | Triggering_RTE err ->
                  add_crash_entry corpus id err
              | Covering_label | Oracle_failure ->
                  Lwt.return ()
            in
            let* () = write_test corpus file v in
            let raw = Lazy.from_fun @@ fun () -> Lwt.return v in
            Tests_table.add tests_cache id { file; raw };
            Lwt.return ()
      end
    end
  end tests_stream

let make ~workspace params =
  let tests_dir = workspace.workdir / "raw" in
  let crashes_file = workspace.workdir / "crashdb" in
  let bypassed_file = workspace.workdir / "bypassed" in
  let* () = Sc_sys.Lwt_file.touch_dir tests_dir
  and* () = Sc_sys.Lwt_file.touch crashes_file
  and* () = Sc_sys.Lwt_file.touch bypassed_file in
  let* bypassed_count =
    Lwt.catch
      (fun () -> let<* ic = bypassed_file in Lwt_io.read_int ic)
      (function Failure _ | End_of_file -> Lwt.return 0 | e -> Lwt.reraise e)
  in
  let tests_mbox = Lwt_mvar.create_empty () in
  let res =
    {
      tests_dir;
      tests_mbox;
      tests_stream = Lwt_stream.from (fun () -> Lwt_mvar.take tests_mbox);
      tests_cache = Tests_table.create 5;
      tests_cache_mutex = Lwt_mutex.create ();
      crashes = Crash_table.create 5;
      crashes' = Tests_table.create 5;
      crashes_file;
      bypassed_count = ref bypassed_count;
      bypassed_count_mutex = Lwt_mutex.create ();
      bypassed_file;
      params;
    }
  in
  let* () = load_known_crash_table res in
  let* () = cache_existing_tests res in
  Lwt.async (fun () -> receive_new_tests res);
  Lwt.return res

let share_test (type r) ?on_share ~outcome ~toolname
    ({ tests_mbox; params; _ }: r corpus) v =
  (* TODO: May need to deal with padding/alignment bytes in v before computing
     the digest *)
  let module Raw_test = (val params.test_repr) in
  let v_str = Raw_test.Val.to_string v in
  let id = Digest.string v_str in
  let* () = match on_share with Some f -> f id | None -> Lwt.return () in
  let* () = Lwt_mvar.put tests_mbox (Some (id, { v; toolname; outcome })) in
  Lwt.return id

let share_test' ?on_share ~outcome ~toolname corpus v =
  let* _ = share_test ?on_share ~outcome ~toolname corpus v in
  Lwt.return ()

let on_new_test ({ tests_dir; tests_cache;
                   tests_cache_mutex; _ } as corpus) func =
  Sc_sys.Lwt_watch.monitor_dir tests_dir
    ~on_close:begin fun f ->
      let* { id; _ } as metadata = test_metadata corpus f in
      let* { file; raw } =
        Lwt_mutex.with_lock tests_cache_mutex begin fun () ->
          Lwt.return @@ try
            Tests_table.find tests_cache id
          with Not_found ->
            let v = {
              file = f;
              raw = Lazy.from_fun @@ fun () -> read_raw_test corpus f;
            } in
            Log.debug "Caching input %s" (Digest.to_hex id);
            Tests_table.add tests_cache id v;
            v
        end
      in
      let link str = Sc_sys.Lwt_file.link file str in
      func { raw; link; metadata }
    end

let stop_receiving_tests { tests_mbox; tests_stream; _ } =
  let* () = Lwt_mvar.put tests_mbox None in
  let* () = Lwt_stream.closed tests_stream in
  Lwt.return ()

let register_one_bypassed_test corpus =
  Lwt_mutex.with_lock corpus.bypassed_count_mutex begin fun () ->
    incr corpus.bypassed_count;
    let>* oc = corpus.bypassed_file in
    Lwt_io.write_int oc !(corpus.bypassed_count)
  end

let info ({ tests_cache; crashes'; bypassed_count;_ } as corpus): info =
  let total = Tests_table.length tests_cache in
  let rte = Tests_table.length crashes' in
  let fails =
    Tests_table.fold begin fun _ { file; _ } ->
      match test_outcome corpus file with
      | Error ()
      | Ok (Covering_label | Triggering_RTE _) -> Fun.id
      | Ok Oracle_failure -> succ
    end tests_cache 0
  in
  {
    num_tests_gen = total - fails - rte;
    num_tests_imported = !bypassed_count;
    num_crash_gen = rte;
    num_crash_imported = 0;                                        (* for now *)
    num_fails_gen = fails;
    num_fails_imported = 0;                                        (* for now *)
  }

let has_crashes { num_crash_gen; num_crash_imported; _ } =
  num_crash_gen > 0 || num_crash_imported > 0

let has_oracle_failures { num_fails_gen; num_fails_imported; _ } =
  num_fails_gen > 0 || num_fails_imported > 0

let test_repr corpus =
  corpus.params.test_repr

let test_struct corpus =
  corpus.params.test_struct

(* --- *)

(* This Main module is included directly in Sc_corpus, so finalize here the
   library's Types module with the corpus type, that we want to keep
   abstract. *)
module Types = struct
  include Types
  type nonrec 'r corpus = 'r corpus
end
