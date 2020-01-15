(lang dune 1.11)
(name tezcheck)

(generate_opam_files true)

(source (uri "git+https://gitlab.com/releaselab/fresco/tezcheck.git"))
(bug_reports "https://gitlab.com/releaselab/fresco/tezcheck/issues")
(homepage "https://gitlab.com/releaselab/fresco/tezcheck")
(license MIT)
(authors "João Santos Reis <joaosreis@outlook.pt>")
(maintainers "João Santos Reis <joaosreis@outlook.pt>")

(package
 (name tezcheck)
 (synopsis "An CFG generator for Tezla programs")
 (depends
  (dune :build)
  (michelson :dev)
  (tezla :dev)
  (tezla_cfg :dev)
  (batteries :dev)
  (fix :dev)))
