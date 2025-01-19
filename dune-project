(lang dune 2.3)

(name tezcheck)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/tezcheck.git"))

(bug_reports "https://github.com/joaosreis/tezcheck/issues")

(homepage "https://github.com/joaosreis/tezcheck")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name tezcheck)
 (synopsis "A CFG generator for Tezla programs")
 (depends
  (dune :build)
  michelson-adt
  michelson-parser
  tezla
  tezla-cfg
  lattices
  (core
   (and (>= v0.14) (< v0.15)))))
