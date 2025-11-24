(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

exception FAILED_JSON_DESTRUCT of {exn: exn; json: string}
exception FAILED_JSON_PARSING of {exn : exn; json: string}
exception UNKNOWN_PROPERTY of string

module OPTIONS = struct

  type mode =
    | Cover
    (** Performs a standard coverage analysis *)
    | Assert
    (** Attemps to prove labels are uncoverable (or returns a counter-example) *)
    | CLabel
    (** Same as [Assert], but uses the clabel mode of CBMC. *)

  (** All the options to configure CBMC from the orchestrator. *)
  type t = {
    mode: mode;
    timeout: float;
    unwind: int;
  }
end

module DATA = struct

  (** The different strategies for handling malloc. *)
  type malloc_may_fail =
    | CantFail
    (** Malloc never fails (by default in CBMC) *)
    | WithFailAssertion
    (** Allows malloc to fail and set malloc failure mode to
        assert-then-assume *)
    | WithNull
    (** Allows malloc to fail and set malloc failure mode to
        return null *)
    | DefaultFail
    (** Allows malloc to fail, but does not specify failure mode. *)

  (** The options for the CBMC tool. They will be passed as a JSON value to the
      tool with the option `--json-interface`.
      https://diffblue.github.io/cbmc//man/cbmc.html
  *)
  type json_options = {
    oarguments: string list; (** The list of files to analyze. *)
    ofunction: string; (** Sets the main function name. *)
    ounwind: int option;
    (** Sets the max number of unwindings. None = infinite unwind *)
    oproperties: string list option;
    (** Restricts the properties to check. None = all properties. *)
    oshow_properties: bool;
    (** Show the properties, but don't run the analysis. *)
    ocover: string option;
    (** Uses a specific coverage criterion. In our case we only use the "cover"
        coverage to aim at specific properties to cover. *)
    oerror_label: string list option;
    (** Checks that labels are uncoverable. *)
    opointer_check: bool;
    (** Enables pointer checks. *)
    onondet_static: bool;
    (** Allows to use non deterministic values for globals, but
        also randomize already set globals -> must be set to false. *)
    omalloc_may_fail: malloc_may_fail;
    (** Allows malloc to fail and specifies fail strategy. *)
  }

  (** Output of the tool. They are received as JSON values.
      All the following types are elements structuring CBMC's answer.
      These types should be abstract, except we need them for testing. *)

  (** A location in the project. *)
  type source_location = {
    slfile: string;
    slfunction: string option;
    slline: string;
    slworking_dir: string option;
  }

  (** A custom message from CBMC. *)
  type message = {
    mtext : string;
    mtyp : string;
    msource_location : source_location option
  }

  type status =
    | Success
    | Failure_
    | Unknown of string

  type 'a cell =
    | ProgramInfo of string
    | Message of message
    | Data of 'a
    | CProverStatus of status

  (* Property listing output *)

  type property_class =
    | ErrorLabel
    | Assertion
    | Coverage
    | Unknown of string

  type property = {
    pclass: property_class;
    pdescription: string;
    pexpression: string;
    pname: string;
    psource_location: source_location;
  }

  (* Analysis output *)

  type base_value =
    | Value of effective_base_value
    | Unknown of (string * string option) (* name * type *)

  and effective_base_value = {
    vbinary: string option;
    vdata: string;
    vname: string;
    vtype: string option;
    vwidth: int option;
  }

  type structure_field = {
    sfname: string;
    sfvalue: value
  }

  and structured = {
    smembers: structure_field list;
    sname: string
  }

  and value =
    | Base of base_value
    | Structured of structured
    | Array of (int * value) list
    | Union of structure_field

  type input = {
    iid: string;
    ivalue: value
  }

  (** Output of cover analysis. *)

  type test = {
    tcovered_goals : string list;
    tinputs: input list;
  }

  type goal = {
    gdescription: string;
    ggoal: string;
    gsource_location: source_location;
    gstatus: status;
  }

  type goals_details = {
    gdgoals: goal list;
    gdgoals_covered: int;
    gdtotal_goals: int;
  }

  type cbmc_cover_output =
    | Goals of goals_details
    | Tests of test list

  (** Output of assert analysis *)

  type function_ = {
    fdisplay_name : string;
    fidentifier : string;
    fsource_location: source_location;
  }

  type location_only = {
    lohidden: bool;
    losource_location: source_location;
    lostep_type: string; (* Should be = to "location-only" *)
    lothread: int;
  }

  type function_info = {
    fifunction : function_;
    fihidden: bool;
    fiinternal: bool;
    fistep_type: string; (* Should be = to "function-call"*)
    fithread: int;
    fisource_location: source_location option;
  }

  type assignment = {
    atype : string;
    ahidden : bool;
    ainternal : bool;
    alhs : string;
    amode: string;
    asource_location: source_location;
    astep_type: string; (* Should be = to "assignment" *)
    athread: int;
    avalue: value;
    areason: string option;
  }

  type input_step = {
    ihidden: bool;
    iinputID: string;
    iinternal: bool;
    imode: string;
    isource_location: source_location;
    istep_type: string; (* Should be = to input *)
    ithread: int;
    ivalues: value list;
  }

  type failure_step = {
    fshidden : bool;
    fsinternal : bool;
    fsproperty : string;
    fsreason : string;
    fssource_location : source_location;
    fsstep_type : string;
    fsthread : int
  }

  type output_step = {
    ohidden: bool;
    ointernal: bool;
    ostep_type: string;
    othread: int;
    omode: string;
    oid: string;
    oloc: source_location;
    ovalues : value list
  }

  type instruction =
    | Function of function_info
    | Assignment of assignment
    | Location of location_only
    | FailureStep of failure_step
    | Input of input_step
    | Output of output_step

  type assertion_check = {
    acdescription : string;
    acproperty : string;
    acsource_location : source_location option;
    acstatus: status;
    actrace: instruction list option;
  }

  type cbmc_assert_output = assertion_check list
end

module PropertyMap = struct
  open DATA

  include Basics.MakeMap(struct
      type t = property
      let compare p1 p2 = String.compare p1.pname p2.pname
      let print ppf p = Fmt.string ppf p.pname
    end)

  let find_by_name n m =
    let res =
      find_last_opt begin fun {pname; _} ->
        pname <= n
      end m
    in
    match res with
    | Some ({pname; _}, _) when pname = n -> res
    | _otherwise -> None


  (* Returns the list of names of the properties in the map *)
  let names m = fold (fun p _ acc -> p.pname :: acc) m []

  let print ?check_equal =
    let pp_check_equal = match check_equal with
      | None -> fun _ _ -> ()
      | Some str -> fun fmt s -> Format.fprintf fmt "@ (%b)" (str = s)
    in
    printkv begin fun ppf ({ pname; _ }, lbl) ->
      Fmt.pf ppf "%i%a" (Sc_C.Cov_label.id lbl)
        pp_check_equal pname
    end
end

type simple_label_env = {
  proof_objectives : Sc_C.Cov_label.simple PropertyMap.t;
  already_proven: Sc_C.Cov_label.simple PropertyMap.t;
  extra_required_properties : DATA.property list;
}

(** The information required for starting CBMC. *)
type 'a analysis_env =
  | SimpleLabelEnv : simple_label_env -> [`simple] analysis_env

type runner_options =
  {
    runner_iteration: int;                    (* corresponds to project run *)
    runner_inputs: Sc_sys.File.dir;
    runner_outputs: Sc_sys.File.dir;
    runner_resdir:Sc_sys.File.dir;
    runner_mode: OPTIONS.mode
  }
