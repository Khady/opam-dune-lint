open OpamTypes

let with_test = OpamVariable.of_string "with-test"
let ocaml = OpamPackage.Name.of_string "ocaml"

(* Before: "foo" {with-test}
   After:  "foo"
 *)
let rec remove_with_test : filter -> filter = function
  | FIdent ([], var, None) when OpamVariable.to_string var = "with-test" -> FBool true
  | FBool _ | FString _ | FIdent _ | FDefined _ | FUndef _ | FNot _ | FOr _ | FOp _ as x -> x
  | FAnd (x, y) -> FAnd (remove_with_test x, remove_with_test y)

let formula_of_filter = function
  | FBool true -> Empty
  | expr -> Atom (Filter expr)

let map_filter f = OpamFormula.map (function
    | Constraint x -> Atom (Constraint x)
    | Filter x -> formula_of_filter (f x)
  )

let apply_with_test_change (formula : filter filter_or_constraint OpamFormula.formula) = function
  | `Remove_with_test _name -> map_filter remove_with_test formula
  | `Add_with_test _name ->
    OpamFormula.ands [
      formula;
      formula_of_filter (FIdent ([], with_test, None))
    ]

let mask_relop rel version =
  OpamFormula.Atom (Constraint (rel, FString version))

let package_formula name condition = OpamFormula.Atom (name, condition)

let ocaml_formula rel version =
  package_formula ocaml (mask_relop rel (OpamPackage.Version.to_string version))

let rec enabled_if_formula = function
  | Dune_rules.Enabled_if.Always -> Empty
  | Dune_rules.Enabled_if.Never -> Empty
  | Dune_rules.Enabled_if.Compare (rel, version) -> ocaml_formula rel version
  | Dune_rules.Enabled_if.And xs -> OpamFormula.ands (List.map enabled_if_formula xs)
  | Dune_rules.Enabled_if.Or xs -> OpamFormula.ors (List.map enabled_if_formula xs)

let dep_atom_formula ~with_test_only dep =
  let version =
    OpamPackage.version_to_string dep
  in
  let constraints =
    if with_test_only then
      OpamFormula.ands [
        mask_relop `Geq version;
        formula_of_filter (FIdent ([], with_test, None));
      ]
    else
      mask_relop `Geq version
  in
  package_formula (OpamPackage.name dep) constraints

let dep_formula ~with_test ~enabled_if dep =
  let dep = dep_atom_formula ~with_test_only:with_test dep in
  if Dune_rules.Enabled_if.is_always enabled_if then
    dep
  else if Dune_rules.Enabled_if.is_never enabled_if then
    Empty
  else
    OpamFormula.Block
      (OpamFormula.Or
         (enabled_if |> Dune_rules.Enabled_if.negate |> enabled_if_formula,
          OpamFormula.Block (OpamFormula.And (enabled_if_formula enabled_if, dep))))

let mentions_name name formula =
  OpamPackage.Name.Set.mem name (OpamFormula.all_names formula)

let find_dep_formula depends name =
  depends
  |> OpamFormula.ands_to_list
  |> List.filter (mentions_name name)
  |> function
  | [] -> None
  | formulas -> Some (OpamFormula.ands formulas)

let compare_filtered_formula =
  OpamFormula.compare_formula Stdlib.compare

let equal_filtered_formula a b =
  compare_filtered_formula a b = 0

let replace_dep_formula depends name dep =
  let others =
    depends
    |> OpamFormula.ands_to_list
    |> List.filter (fun formula -> not (mentions_name name formula))
  in
  OpamFormula.ands (others @ [dep])

let update_depends (depends : filtered_formula) = function
  | `Add_build_dep dep ->
    OpamFormula.And (depends, dep_formula ~with_test:false ~enabled_if:Dune_rules.Enabled_if.always dep)
  | `Add_test_dep dep ->
    OpamFormula.ands [depends; dep_formula ~with_test:true ~enabled_if:Dune_rules.Enabled_if.always dep]
  | `Remove_with_test name | `Add_with_test name as change ->
    let update (name2, formula) =
      if name <> name2 then OpamFormula.Atom (name2, formula)
      else OpamFormula.Atom (name, apply_with_test_change formula change)
    in
    OpamFormula.map update depends
  | `Set_dep_formula (name, dep) ->
    replace_dep_formula depends name dep

let rec flatten : _ OpamFormula.formula -> _ list = function
  | Empty -> []
  | Atom (name, f) -> [(OpamPackage.Name.to_string name, f)]
  | Block x -> flatten x
  | And (x, y) -> flatten x @ flatten y
  | Or (x, y) -> flatten x @ flatten y

(* with-test dependencies are not available in the plain build environment. *)
let build_env x =
  match OpamVariable.Full.to_string x with
  | "with-test" -> Some (OpamTypes.B false)
  | _ -> None

let available_in_build_env =
  let open OpamTypes in function
  | Filter f -> OpamFilter.eval_to_bool ~default:true build_env f
  | Constraint _ -> true

let classify deps : [`Build | `Test] OpamPackage.Name.Map.t =
  flatten deps
  |> List.fold_left (fun acc (name, formula) ->
      let ty = if OpamFormula.eval available_in_build_env formula then `Build else `Test in
      let update x = match x, ty with
        | `Build, `Build | `Test, `Test -> x
        | `Test, `Build | `Build, `Test -> `Build
      in
      OpamPackage.Name.Map.update (OpamPackage.Name.of_string name) update ty acc
    ) OpamPackage.Name.Map.empty
