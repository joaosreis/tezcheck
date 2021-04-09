(lang dune 2.3)

(name tezcheck)

(generate_opam_files true)

(source
 (uri "git+https://gitlab.com/releaselab/fresco/tezcheck.git"))

(bug_reports "https://gitlab.com/releaselab/fresco/tezcheck/issues")

(homepage "https://gitlab.com/releaselab/fresco/tezcheck")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name tezcheck)
 (synopsis "A CFG generator for Tezla programs")
 (depends
  (dune :build)
  michelson
  tezla
  tezla-cfg
  core_kernel))
