open Types

module Copy_rules = struct

  let sexp_of_file file =
    try Sexp.load_sexps @@ Fpath.to_string file with
    | Sexp.Parse_error _ as e ->
      (Fmt.pr "Error parsing 'dune file' output:\n"; raise e)

  type t =
    {
      target: string;
      from_name: string;
      to_name: string;
      dep: string;
      package: string
    }

  let dump_copy = {
    target = "";
    from_name = "";
    to_name = "";
    dep = "";
    package = ""
  }

  let rules = Hashtbl.create 10

  let copy_rules_of_sexp sexps =
    let is_action_copy sexp =
      sexp
      |> (function
          | Sexp.List l -> if List.mem (Sexp.Atom "rule") l then l else []
          | _ -> [])
      |> List.exists (function
          | Sexp.List [ Atom "action"; List [ Atom "copy"; _; _]] -> true
          | _ -> false)
    in
    let copy_rule_of_sexp sexp =
      match sexp with
      | Sexp.List sexps ->
        List.fold_left (fun copy _sexp ->
            match _sexp with
            | Sexp.List [Atom "action"; List [ _; Atom f; Atom t]] ->
              {{copy with from_name = f } with to_name = t}
            | Sexp.List [Atom "deps"; List [Atom "package"; Atom s]]-> {copy with package = s}
            | Sexp.List [Atom "deps"; List [Atom "package"; Atom p]; Atom d]
            | Sexp.List [Atom "deps"; Atom d; List [Atom "package"; Atom p]] ->
              {{copy with package = p} with dep = d}
            | Sexp.List [Atom "deps"; Atom s]    -> {copy with dep = s}
            | Sexp.List [Atom "target"; Atom s]  -> {copy with target = s}
            | Sexp.Atom "rule"                   -> copy
            | _ -> copy
          ) dump_copy sexps
      | s -> Fmt.failwith "%s is not a rule" (Sexp.to_string s)
    in
    sexps
    |> List.filter_map (fun rule ->
        if not (is_action_copy rule) then
          None
        else
          rule
          |> copy_rule_of_sexp
          |> fun copy ->
          if String.equal copy.to_name "%{target}" && String.equal copy.from_name "%{deps}" then
            (*when we got `(action (copy %{deps} %{target}))` *)
            Some {{copy with to_name = copy.target} with from_name = copy.dep}
          else Some copy)

  let copy_rules_map =
    List.fold_left (fun map copy -> Item_map.add copy.from_name copy map) Item_map.empty

  let get_copy_rules file =
    match Hashtbl.find_opt rules file with
    | None when OS.Path.exists file |> Stdlib.Result.get_ok ->
      let copy_rules = copy_rules_of_sexp (sexp_of_file file) in
      Hashtbl.add rules file copy_rules; copy_rules
    | None -> Hashtbl.add rules file []; []
    | Some copy_rules -> copy_rules

  let find_dest_name ~name rules =
    let rec find_dest_name name rules =
      match Item_map.find_opt name rules with
      | None   -> Some name
      | Some t -> find_dest_name t.to_name rules
    in
    match Item_map.find_opt name rules with
    | None -> None (* Not found in the first step *)
    | Some t -> find_dest_name t.to_name rules
end

module Enabled_if = struct
  type kind = Lib | Exe | Test

  module Key = struct
    type t = kind * string

    let compare = Stdlib.compare
  end

  module Key_map = Map.Make(Key)

  type t =
    | Always
    | Never
    | Compare of OpamTypes.relop * OpamPackage.Version.t
    | And of t list
    | Or of t list

  let always = Always
  let never = Never

  let is_always = function
    | Always -> true
    | Never | Compare _ | And _ | Or _ -> false

  let is_never = function
    | Never -> true
    | Always | Compare _ | And _ | Or _ -> false

  let rel_of_string = function
    | "=" -> `Eq
    | "<>" | "!=" -> `Neq
    | ">=" -> `Geq
    | ">" -> `Gt
    | "<=" -> `Leq
    | "<" -> `Lt
    | op -> Fmt.failwith "Unsupported enabled_if operator %S" op

  let flip_rel = function
    | `Eq -> `Eq
    | `Neq -> `Neq
    | `Geq -> `Leq
    | `Gt -> `Lt
    | `Leq -> `Geq
    | `Lt -> `Gt

  let rec normalize = function
    | Always | Never | Compare _ as t -> t
    | And xs ->
      let xs =
        xs
        |> List.map normalize
        |> List.concat_map (function And ys -> ys | y -> [y])
        |> List.filter (fun x -> not (is_always x))
      in
      if List.exists is_never xs then Never
      else
        (match xs with
         | [] -> Always
         | [x] -> x
         | _ -> And xs)
    | Or xs ->
      let xs =
        xs
        |> List.map normalize
        |> List.concat_map (function Or ys -> ys | y -> [y])
        |> List.filter (fun x -> not (is_never x))
      in
      if List.exists is_always xs then Always
      else
        (match xs with
         | [] -> Never
         | [x] -> x
         | _ -> Or xs)

  let and_ xs = normalize (And xs)

  let or_ xs = normalize (Or xs)

  let negate =
    let rec aux = function
      | Always -> Never
      | Never -> Always
      | Compare (rel, version) -> Compare (OpamFormula.neg_relop rel, version)
      | And xs -> Or (List.map aux xs)
      | Or xs -> And (List.map aux xs)
    in
    fun t -> aux t |> normalize

  let equal a b = Stdlib.compare (normalize a) (normalize b) = 0

  let rec mentions_ocaml_version = function
    | Sexp.Atom "%{ocaml_version}" -> true
    | Sexp.List xs -> List.exists mentions_ocaml_version xs
    | Sexp.Atom _ -> false

  let version_of_atom = function
    | Sexp.Atom version -> OpamPackage.Version.of_string version
    | sexp -> Fmt.failwith "Unsupported enabled_if version atom %a" Sexp.pp_hum sexp

  let rec of_sexp sexp =
    match sexp with
    | Sexp.Atom "true" -> Always
    | Sexp.Atom "false" -> Never
    | Sexp.List (Sexp.Atom "and" :: xs) ->
      and_ (List.map of_sexp xs)
    | Sexp.List (Sexp.Atom "or" :: xs) ->
      or_ (List.map of_sexp xs)
    | Sexp.List [Sexp.Atom "not"; x] ->
      negate (of_sexp x)
    | Sexp.List [Sexp.Atom rel; lhs; rhs] ->
      let rel = rel_of_string rel in
      begin match lhs, rhs with
      | Sexp.Atom "%{ocaml_version}", version ->
        Compare (rel, version_of_atom version)
      | version, Sexp.Atom "%{ocaml_version}" ->
        Compare (flip_rel rel, version_of_atom version)
      | _ ->
        Fmt.failwith "Unsupported ocaml_version enabled_if expression %a" Sexp.pp_hum sexp
      end
    | _ ->
      Fmt.failwith "Unsupported enabled_if expression %a" Sexp.pp_hum sexp

  let of_enabled_if_sexp = function
    | None -> Always
    | Some sexp ->
      if mentions_ocaml_version sexp then of_sexp sexp else Always

  let names_of_item sexps =
    let names_of_field = function
      | Sexp.List [Sexp.Atom "name"; Sexp.Atom name]
      | Sexp.List [Sexp.Atom "public_name"; Sexp.Atom name] -> Some [name]
      | Sexp.List [Sexp.Atom "names"; Sexp.List names]
      | Sexp.List [Sexp.Atom "public_names"; Sexp.List names] ->
        Some (List.map (function Sexp.Atom name -> name | sexp -> Fmt.failwith "Unexpected item name %a" Sexp.pp_hum sexp) names)
      | _ -> None
    in
    sexps
    |> List.filter_map names_of_field
    |> List.flatten

  let enabled_if_of_item sexps =
    List.find_map (function
        | Sexp.List [Sexp.Atom "enabled_if"; expr] -> Some expr
        | _ -> None
      ) sexps
    |> of_enabled_if_sexp

  let key_map_of_sexps sexps =
    List.fold_left (fun acc -> function
        | Sexp.List (Sexp.Atom head :: items) ->
          let kind =
            match head with
            | "library" -> Some Lib
            | "executable" | "executables" -> Some Exe
            | "test" | "tests" -> Some Test
            | _ -> None
          in
          begin match kind with
          | None -> acc
          | Some kind ->
            let enabled_if = enabled_if_of_item items in
            names_of_item items
            |> List.fold_left (fun acc name -> Key_map.add (kind, name) enabled_if acc) acc
          end
        | _ -> acc
      ) Key_map.empty sexps

  let cache = Hashtbl.create 16

  let key_map_of_file file =
    match Hashtbl.find_opt cache file with
    | Some key_map -> key_map
    | None ->
      let key_map = key_map_of_sexps (Copy_rules.sexp_of_file file) in
      Hashtbl.add cache file key_map;
      key_map

  let get_enabled_if ~kind ~source_dir ~name =
    let file = Fpath.(source_dir / "dune") in
    if OS.Path.exists file |> Stdlib.Result.get_ok then
      key_map_of_file file
      |> Key_map.find_opt (kind, name)
      |> Option.value ~default:Always
    else
      Always
end
