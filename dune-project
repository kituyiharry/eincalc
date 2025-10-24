(lang dune 3.20)

(name eincalc)

(generate_opam_files true)

(source
 (github kituyiharry/eincalc))

(authors "Harry K <kituyiharry+github@gmail.com>")

(maintainers "Harry K <kituyiharry+github@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name eincalc)
 (synopsis "A Sheet for Shape Rotators")
 (description "A sheet for shape rotators")
 (tags
  ("spreadsheet" "ndarray"))

 (depends
   ; Required dependencies
   (ocaml (>= 5.0.0))
   (fungi (>= 0.1.4))
   ;(dune  (>= 3.12))

   ; Optional dependencies with versions
   ; base
   (eincalc          :with-test)
   (ounit2          :with-test)
   (qcheck-core     :with-test)
   (core_bench      :with-test)
   ;(ppx_jane :with-test)  ; Only needed for running tests
   ;(odoc :with-doc)       ; Only needed for documentation generation
   ;(mdx :with-test)       ; Only needed if you use mdx for doc tests

   ; Add any other dependencies your library needs
   )
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
