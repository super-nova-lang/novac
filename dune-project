(lang dune 3.20)

(name novac)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name novac)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml easy_logging spectrum ppx_deriving ppx_inline_test)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
