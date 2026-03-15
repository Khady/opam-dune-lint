open OpamTypes

val classify : filtered_formula -> [`Build | `Test] OpamPackage.Name.Map.t

val dep_formula :
  with_test:bool ->
  enabled_if:Dune_rules.Enabled_if.t ->
  package ->
  filtered_formula

val find_dep_formula :
  filtered_formula ->
  name ->
  filtered_formula option

val equal_filtered_formula :
  filtered_formula ->
  filtered_formula ->
  bool

val update_depends :
  filtered_formula ->
  [< `Add_build_dep of package
  | `Add_test_dep of package
  | `Add_with_test of name
  | `Remove_with_test of name
  | `Set_dep_formula of name * filtered_formula ] ->
  filtered_formula
