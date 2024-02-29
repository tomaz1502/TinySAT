# TinySAT

A tiny proof producing SAT solver. The goal of this project is to help me learn OCaml
and the process of modifying an algorithm to produce proof certificate of its results.
Currently we are working on doing this modification on the brute force search and on
DPLL.

## Road map:
  - [x] Parse DIMACS.
  - [x] Implement brute force.
  - [x] Implement DPLL.
  - [ ] Create a functor to parametrize algo modules by the input format.
  - [ ] Produce proofs in the brute force search.
  - [ ] Produce proofs in DPLL.
  - [ ] Tests? Maybe checking the proofs?
  - [ ] Add more algorithms. Ideas: BDD, CDCL.
  - [ ] Perhaps a little web interface to input formulas, to learn something about web programming
