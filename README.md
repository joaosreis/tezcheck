# Tezcheck

Tezcheck is a static analysis tool for Michelson smart contracts that takes
advantage of [Tezla](https://gitlab.com/releaselab/fresco/tezla)
intermediate representation and [SoftCheck](https://gitlab.com/joaosreis/softcheck).

## Requirements

* [Michelson parser and ADT](https://gitlab.com/releaselab/fresco/michelson)
* [Tezla](https://gitlab.com/releaselab/fresco/tezla)
* [Tezla CFG generator](https://gitlab.com/releaselab/fresco/tezla-cfg)

## Install instructions

### Using dune

```bash
git clone https://gitlab.com/releaselab/fresco/tezcheck.git
cd tezcheck
dune build @install
dune install
```

### Using opam
```bash
opam pin add tezcheck https://gitlab.com/releaselab/fresco/tezcheck.git
```

---

Developed under the [FRESCO](https://release.di.ubi.pt/projects/fresco.html)
project (Formal Verification of Smart Contracts), generously funded by [Tezos
Foundation](https://tezos.foundation).
