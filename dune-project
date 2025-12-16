(lang dune 3.20)

(name novac)

(generate_opam_files true)

(source
 (github super-nova-lang/novac))

(authors "Ashton Sickler <ashton.sickler06@gmail.com>")

(maintainers "Ashton Sickler <ashton.sickler06@gmail.com>")

(license LICENSE)

(package
 (name novac)
 (synopsis "Supernova Compiler")
 (description "Supernova Compiler")
 (depends
  ocaml
  easy_logging
  spectrum
  ppx_deriving
  ppx_inline_test)
 (tags
  ("compiler" "supernova" "novac")))
