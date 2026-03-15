Create a simple dune project with an OCaml-version-gated public library:

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (package
  >  (name test)
  >  (synopsis "Test package"))
  > EOF

  $ cat > test.opam << EOF
  > # Preserve comments
  > opam-version: "2.0"
  > synopsis: "Test package"
  > build: [
  >   ["dune" "build"]
  > ]
  > depends: [
  >   "base"
  > ]
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name base_lib)
  >  (public_name test.base_lib)
  >  (modules base_lib)
  >  (libraries base))
  > 
  > (library
  >  (name ocaml5_lib)
  >  (public_name test.ocaml5_lib)
  >  (modules ocaml5_lib)
  >  (libraries fmt)
  >  (enabled_if (>= %{ocaml_version} 5.2.0)))
  > 
  > (library
  >  (name ocaml5_lib_2)
  >  (public_name test.ocaml5_lib_2)
  >  (modules ocaml5_lib_2)
  >  (libraries fmt)
  >  (enabled_if (>= %{ocaml_version} 5.2.0)))
  > EOF

  $ echo 'let x = 1' > base_lib.ml
  $ echo 'let x = 2' > ocaml5_lib.ml
  $ echo 'let x = 3' > ocaml5_lib_2.ml
  $ dune build

Replace all version numbers with "1.0" to get predictable output.

  $ export OPAM_DUNE_LINT_TESTS=y

Check that the OCaml-version-gated dependency is preserved as a conditional opam formula:

  $ opam-dune-lint -f
  test.opam: changes needed:
    ("ocaml" {< "5.2.0"} | ("ocaml" {>= "5.2.0"} & "fmt" {>= "1.0"})) [from /]
  Note: version numbers are just suggestions based on the currently installed version.
  Wrote "./test.opam"
  Warning in test: The package has a dune-project file but no explicit dependency on dune was found.

  $ cat test.opam | sed 's/= [^&)}]*/= */g'
  # Preserve comments
  opam-version: "2.0"
  synopsis: "Test package"
  build: [
    ["dune" "build"]
  ]
  depends: [
    "base"
    ("ocaml" {< "5.2.0"} | ("ocaml" {>= *} & "fmt" {>= *}))
  ]

  $ opam-dune-lint
  test.opam: OK
  Warning in test: The package has a dune-project file but no explicit dependency on dune was found.

Equivalent but noisier conditional formulas should be rewritten back to the canonical form:

  $ cat > test.opam << EOF
  > # Preserve comments
  > opam-version: "2.0"
  > synopsis: "Test package"
  > build: [
  >   ["dune" "build"]
  > ]
  > depends: [
  >   "base"
  >   ("ocaml" {< "5.2.0"} & "ocaml" {< "5.2.0"} | (("ocaml" {>= "5.2.0"} | "ocaml" {>= "5.2.0"}) & "fmt" {>= "1.0"}))
  > ]
  > EOF

  $ opam-dune-lint -f
  test.opam: changes needed:
    ("ocaml" {< "5.2.0"} | ("ocaml" {>= "5.2.0"} & "fmt" {>= "1.0"})) [from /]
  Note: version numbers are just suggestions based on the currently installed version.
  Wrote "./test.opam"
  Warning in test: The package has a dune-project file but no explicit dependency on dune was found.

  $ cat test.opam | sed 's/= [^&)}]*/= */g'
  # Preserve comments
  opam-version: "2.0"
  synopsis: "Test package"
  build: [
    ["dune" "build"]
  ]
  depends: [
    "base"
    ("ocaml" {< "5.2.0"} | ("ocaml" {>= *} & "fmt" {>= *}))
  ]
