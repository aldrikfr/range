(lang dune 2.0)
(name range)
(version "0.8.2")
(source (github aldrikfr/range))
(license GPL-3.0-or-later)
(authors "Aldrik KLEBER")
(maintainers "contact@aldrik.net")
(generate_opam_files true)

(package
 (name range)
 (synopsis "Fold on integer range")
 (description "
Library to work on range of sequential integers with features like : split, map,
filter capacities.
Designed for making distributed computing easier with libraries like functory.
")
 (depends
  (ocaml (>= 4.02) )
  (base (and (>= 0.12) (<= 0.14)) )
  (dune (>= 1.11) )
  (ounit :with-test)
  ))
