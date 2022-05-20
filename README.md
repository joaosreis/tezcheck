# Tezcheck

![main workflow](https://github.com/joaosreis/tezcheck/actions/workflows/main.yml/badge.svg)

Tezcheck is a static analysis tool for Michelson smart contracts that takes
advantage of [Tezla](https://github.com/joaosreis/tezla)
intermediate representation and [SoftCheck](https://github.com/joaosreis/softcheck).

## Install instructions

### Using dune

```bash
git clone https://github.com/joaosreis/tezcheck.git
cd tezcheck
dune build @install
dune install
```

### Using opam

```bash
opam install https://github.com/joaosreis/tezcheck.git
```

---

Developed under the [FRESCO](https://release.di.ubi.pt/projects/fresco.html)
project (Formal Verification of Smart Contracts), generously funded by [Tezos
Foundation](https://tezos.foundation).
