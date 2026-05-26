(lang dune 3.22)

(name revpn-ccsk)

(generate_opam_files true)

(source
 (github danieldavalos93/revpn-ccsk-ocaml))

(authors "Daniel Dávalos <davalos@uba.ar>")

(maintainers "Daniel Dávalos <davalos@uba.ar>")

(license LICENSE)

(documentation https://danieldavalos93.github.io/revpn-ccsk-ocaml)

(package
 (name revpn-ccsk)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("Petri nets" "CCSK" "algorithm")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
