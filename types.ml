module Dir_set = Set.Make(Fpath)

module Paths = Map.Make(String)

module Libraries = Map.Make(String)

module Dir_map = Map.Make(String)

module Item_map = Map.Make(String)

module Sexp = Sexplib.Sexp

module Stdune = Stdune

include Bos

module Change = struct
  type t =
    [ `Remove_with_test of OpamPackage.Name.t
    | `Add_with_test of OpamPackage.Name.t
    | `Add_build_dep of OpamPackage.t
    | `Add_test_dep of OpamPackage.t
    | `Set_dep_formula of OpamPackage.Name.t * OpamTypes.filtered_formula ]
end

module List = struct
  include List
  let rec concat_map f = function
    | [] -> []
    | x::xs -> prepend_concat_map (f x) f xs
  and prepend_concat_map ys f xs =
    match ys with
    | [] -> concat_map f xs
    | y::ys -> y::prepend_concat_map ys f xs
  let find_map f l =
    let rec find f = function
      | [] -> None
      | x::tl -> let v = f x in if Option.is_some v then v else find f tl
    in find f l
end

module String = struct
  include String
  let cat = (^)
end

module Change_with_hint = struct
  type t = Change.t * Dir_set.t

  let pp_name = Fmt.using OpamPackage.Name.to_string Fmt.(quote string)

  let version_to_string =
    if Sys.getenv_opt "OPAM_DUNE_LINT_TESTS" = Some "y" then Fun.const "1.0"
    else OpamPackage.version_to_string

  let includes_version (c, _) =
    match c with
    | `Remove_with_test _
    | `Add_with_test _ -> false
    | `Add_build_dep _
    | `Add_test_dep _
    | `Set_dep_formula _ -> true

  let rec string_of_filter = function
    | OpamTypes.FBool true -> "true"
    | OpamTypes.FBool false -> "false"
    | OpamTypes.FString s -> Fmt.str "%S" s
    | OpamTypes.FIdent ([], var, None) -> OpamVariable.to_string var
    | OpamTypes.FIdent _ -> "<filter>"
    | OpamTypes.FDefined filter -> Fmt.str "?%s" (string_of_filter filter)
    | OpamTypes.FUndef filter -> Fmt.str "!?%s" (string_of_filter filter)
    | OpamTypes.FNot filter -> Fmt.str "!%s" (string_of_filter filter)
    | OpamTypes.FAnd (x, y) -> Fmt.str "%s & %s" (string_of_filter x) (string_of_filter y)
    | OpamTypes.FOr (x, y) -> Fmt.str "%s | %s" (string_of_filter x) (string_of_filter y)
    | OpamTypes.FOp (x, rel, y) ->
      Fmt.str "%s %s %s" (string_of_filter x) (OpamFormula.string_of_relop rel) (string_of_filter y)

  let string_of_filtered_formula formula =
    let mask_version name version =
      if Sys.getenv_opt "OPAM_DUNE_LINT_TESTS" = Some "y"
         && OpamPackage.Name.to_string name <> "ocaml"
      then
        "1.0"
      else
        version
    in
    let string_of_condition name =
      OpamFormula.string_of_formula (function
          | OpamTypes.Constraint (rel, OpamTypes.FString version) ->
            Fmt.str "%s %S" (OpamFormula.string_of_relop rel) (mask_version name version)
          | OpamTypes.Constraint (rel, filter) ->
            Fmt.str "%s %s" (OpamFormula.string_of_relop rel) (string_of_filter filter)
          | OpamTypes.Filter filter -> string_of_filter filter)
    in
    OpamFormula.string_of_formula (fun (name, condition) ->
        let name_quoted = Fmt.str "%a" pp_name name in
        match condition with
        | OpamFormula.Empty -> name_quoted
        | _ -> Fmt.str "%s {%s}" name_quoted (string_of_condition name condition))
      formula

  let pp f (c, dirs) =
    let dirs =
      Dir_set.map (fun path -> if Fpath.is_current_dir path then Fpath.v "/" else path) dirs
    in
    let change, hint =
      match c with
      | `Remove_with_test name -> Fmt.str "%a" pp_name name, ["(remove {with-test})"]
      | `Add_with_test name -> Fmt.str "%a {with-test}" pp_name name, ["(missing {with-test} annotation)"]
      | `Add_build_dep dep -> Fmt.str "%a {>= \"%s\"}" pp_name (OpamPackage.name dep) (version_to_string dep), []
      | `Add_test_dep dep -> Fmt.str "%a {with-test & >= \"%s\"}" pp_name (OpamPackage.name dep) (version_to_string dep), []
      | `Set_dep_formula (_name, formula) -> string_of_filtered_formula formula, []
    in
    let hint =
      if Dir_set.is_empty dirs then hint
      else Fmt.str "[from @[<h>%a@]]" Fmt.(list ~sep:comma Fpath.pp) (Dir_set.elements dirs) :: hint
    in
    if hint = [] then
      Fmt.string f change
    else
      Fmt.pf f "@[<h>%-40s %a@]" change Fmt.(list ~sep:sp string) hint

  let remove_hint (t:t) = fst t
end

let or_die = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let sexp cmd =
  Bos.OS.Cmd.run_out (cmd)
  |> Bos.OS.Cmd.to_string
  |> or_die
  |> String.trim
  |> (fun s ->
      try Sexp.of_string s with
      | Sexp.Parse_error _ as e ->
        Fmt.epr "Error parsing '%s' output:\n" (Bos.Cmd.to_string cmd); raise e)

let csexp cmd =
  Bos.OS.Cmd.run_out (cmd)
  |> Bos.OS.Cmd.to_string
  |> or_die
  |> String.trim
  |> (fun s ->
    match Csexp.parse_string_many s with
    | Ok csexp -> csexp
    | Error msg ->
      Fmt.epr "Error parsing '%s' output:\n%S" (Bos.Cmd.to_string cmd) (snd msg); exit 1)
