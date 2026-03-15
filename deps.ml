open Types
open Dune_items

type dep =
  {
    dirs: Dir_set.t;
    enabled_if: Dune_rules.Enabled_if.t;
  }

type t = dep Libraries.t

let dune_describe_external_lib_deps () = Bos.Cmd.(v "dune" % "describe" % "external-lib-deps")

let dune_describe_entries () = Bos.Cmd.(v "dune" % "describe" % "package-entries")

let describe_external_lib_deps =
  Lazy.from_fun (fun _ ->
      sexp @@ dune_describe_external_lib_deps ()
      |> Describe_external_lib.describe_extern_of_sexp)

let describe_bin_of_entries =
  Lazy.from_fun (fun _ ->
      sexp @@ dune_describe_entries ()
      |> Describe_entries.entries_of_sexp
      |> Describe_entries.items_bin_of_entries)

let has_dune_subproject path =
  if Fpath.is_current_dir path then false
  else
    Fpath.(path / "dune-project")
    |> Bos.OS.Path.exists
    |> Stdlib.Result.get_ok


let parent_path path = if Fpath.is_current_dir path then None else Some (Fpath.parent path)

let rec should_use_dir ~dir_types path =
  match Hashtbl.find_opt dir_types path with
  | Some x -> x
  | None ->
    let r =
      match parent_path path with
      | Some parent ->
        if should_use_dir ~dir_types parent then (
          not (has_dune_subproject path)
        ) else false
      | None ->
        not (has_dune_subproject path)
    in
    Hashtbl.add dir_types path r;
    r

let copy_rules () =
  Lazy.force describe_external_lib_deps
  |> List.concat_map
    (fun d_item ->
       d_item
       |> Describe_external_lib.get_item
       |> (fun (item:Describe_external_lib.item) -> Fpath.(item.source_dir / "dune"))
       |> Dune_rules.Copy_rules.get_copy_rules)
  |> Dune_rules.Copy_rules.copy_rules_map

let bin_of_entries () = Lazy.force describe_bin_of_entries

let is_bin_name_of_describe_lib bin_name (item:Describe_external_lib.item) =
  item.extensions
  |> List.exists (fun extension ->
      String.equal bin_name (String.cat item.name extension))

let find_package_of_exe (item:Describe_external_lib.item) =
  match item.package with
  | Some p -> Some p
  | None ->
    (* Only allow for private executables to find the package *)
    item.extensions
    |> List.find_map (fun extension ->
        Option.map
          (fun bin_name ->
             Option.map
               (fun (item:Describe_entries.item) -> item.package) (Item_map.find_opt bin_name @@ bin_of_entries ()))
          (Dune_rules.Copy_rules.find_dest_name ~name:(String.cat item.name extension) @@ copy_rules ()))
    |> Option.join

let resolve_internal_deps d_items items_pkg =
  (* After the d_items are filtered to the corresponding package request,
   * we need to include the internal_deps in order to reach all the deps.
   * If the internal dep is a public library we skip the recursive resolve
   * because it will be resolve with separate request *)
  let open Describe_external_lib in
  let get_name = function
    | Lib item  -> String.cat item.name ".lib"
    | Exe item  -> String.cat item.name ".exe"
    | Test item -> String.cat item.name ".test"
  in
  let d_items_lib =
    d_items
    |> List.filter_map (fun d_item ->
        if is_lib_item d_item then
          let item = get_item d_item in
          Some (item.Describe_external_lib.name ^ ".lib", Lib item)
        else None)
    |> List.to_seq |> Hashtbl.of_seq
  in
  let rec add_internal acc = function
    | [] -> Hashtbl.to_seq_values acc |> List.of_seq
    | item::tl ->
      if Hashtbl.mem acc (get_name item) then
        add_internal acc tl
      else begin
        Hashtbl.add acc (get_name item) item;
        (get_item item).internal_deps
        |> List.filter_map (fun (name, _) ->
            match Hashtbl.find_opt d_items_lib (String.cat name ".lib") with
            | None -> None
            | Some d_item_lib ->
              if Option.is_some (get_item d_item_lib).package then None
              else Some d_item_lib)
        |> fun internals -> add_internal acc (tl @ internals)
      end
  in
  add_internal (Hashtbl.create 10) items_pkg

let d_item_with_enabled_if d_item =
  let kind, wrap =
    match d_item with
    | Describe_external_lib.Lib _ -> Dune_rules.Enabled_if.Lib, (fun item -> Describe_external_lib.Lib item)
    | Describe_external_lib.Exe _ -> Dune_rules.Enabled_if.Exe, (fun item -> Describe_external_lib.Exe item)
    | Describe_external_lib.Test _ -> Dune_rules.Enabled_if.Test, (fun item -> Describe_external_lib.Test item)
  in
  let item = Describe_external_lib.get_item d_item in
  let enabled_if =
    Dune_rules.Enabled_if.get_enabled_if
      ~kind
      ~source_dir:item.source_dir
      ~name:item.name
  in
  wrap { item with enabled_if }

let get_dune_items dir_types ~pkg ~target =
  let d_items =
    Lazy.force describe_external_lib_deps
    |> List.map (fun d_item ->
        let item = Describe_external_lib.get_item d_item in
        if Describe_external_lib.is_exe_item d_item && Option.is_none item.package
        then
          match find_package_of_exe item with
          | None -> d_item
          | Some pkg -> Describe_external_lib.Exe { item with package = Some pkg }
        else d_item)
  in
  let unresolved_entries =
      let exe_items =
        List.filter_map (function
            | Describe_external_lib.Exe item -> Some item
            | _ -> None) d_items
      in
        bin_of_entries ()
        |> Item_map.partition (fun _ (entry:Describe_entries.item) ->
            exe_items
            |> List.exists
              (fun (item:Describe_external_lib.item) ->
                 is_bin_name_of_describe_lib  entry.bin_name item
                 && Option.equal String.equal (Some entry.package) item.package))
        |> snd
  in
  let d_items =
    d_items
    |> List.map (function
        | Describe_external_lib.Exe item as d_item ->
          item.extensions
          |> List.find_map (fun extension ->
              Item_map.find_opt (String.cat item.name extension) unresolved_entries)
          |> (function
              | None -> d_item
              | Some entry -> Describe_external_lib.Exe { item with package = Some entry.package })
        | d_item -> d_item)
    |> List.map d_item_with_enabled_if
    |> List.filter (fun item ->
        match (item,target) with
        | Describe_external_lib.Test _, `Install -> false
        | Describe_external_lib.Test _, `Runtest -> true
        | _ , `Runtest -> false
        | _, `Install -> true)
    |> List.filter (fun d_item -> should_use_dir ~dir_types (Describe_external_lib.get_item d_item).source_dir)
  in
  List.filter (fun d_item ->
      let item = Describe_external_lib.get_item d_item in
      (* if an item has no package, we assume it's used for testing *)
      if target = `Install then
        Option.equal String.equal (Some pkg) item.package
      else
        Option.equal String.equal (Some pkg) item.package || Option.is_none item.package) d_items
  |> resolve_internal_deps d_items

let item_name = function
  | Describe_external_lib.Lib item -> item.name ^ ".lib"
  | Describe_external_lib.Exe item -> item.name ^ ".exe"
  | Describe_external_lib.Test item -> item.name ^ ".test"

let lib_deps ~pkg ~target =
  let d_items = get_dune_items (Hashtbl.create 10) ~pkg ~target in
  let items_by_name =
    d_items
    |> List.filter_map (function
        | Describe_external_lib.Lib item as d_item -> Some (item.name ^ ".lib", d_item)
        | _ -> None)
    |> List.to_seq
    |> Hashtbl.of_seq
  in
  let resolved = Hashtbl.create 16 in
  let rec add_items = function
    | [] -> ()
    | (d_item, inherited_enabled_if) :: rest ->
      let item = Describe_external_lib.get_item d_item in
      let enabled_if =
        Dune_rules.Enabled_if.and_ [inherited_enabled_if; item.enabled_if]
      in
      if Dune_rules.Enabled_if.is_never enabled_if then
        add_items rest
      else
        let name = item_name d_item in
        let previous =
          match Hashtbl.find_opt resolved name with
          | None -> Dune_rules.Enabled_if.never
          | Some (_, enabled_if) -> enabled_if
        in
        let merged_enabled_if = Dune_rules.Enabled_if.or_ [previous; enabled_if] in
        if Dune_rules.Enabled_if.equal previous merged_enabled_if then
          add_items rest
        else begin
          Hashtbl.replace resolved name (d_item, merged_enabled_if);
          let internal_items =
            item.internal_deps
            |> List.filter_map (fun (name, _) ->
                match Hashtbl.find_opt items_by_name (name ^ ".lib") with
                | None -> None
                | Some d_item_lib ->
                  if Option.is_some (Describe_external_lib.get_item d_item_lib).package then None
                  else Some (d_item_lib, merged_enabled_if))
          in
          add_items (internal_items @ rest)
        end
  in
  add_items (List.map (fun d_item -> d_item, Dune_rules.Enabled_if.always) d_items);
  Hashtbl.to_seq_values resolved
  |> List.of_seq
  |> List.fold_left (fun libs (d_item, enabled_if) ->
      let item = Describe_external_lib.get_item d_item in
      List.fold_left (fun acc (lib, _) ->
          if Astring.String.take ~sat:((<>) '.') lib <> pkg then
            let dep =
              match Libraries.find_opt lib acc with
              | None ->
                { dirs = Dir_set.singleton item.source_dir; enabled_if }
              | Some dep ->
                {
                  dirs = Dir_set.add item.source_dir dep.dirs;
                  enabled_if = Dune_rules.Enabled_if.or_ [dep.enabled_if; enabled_if];
                }
            in
            Libraries.add lib dep acc
          else
            acc)
        libs item.external_deps)
    Libraries.empty

let get_external_lib_deps ~pkg ~target : t = lib_deps ~pkg ~target
