# Wonton

Wonton will be a fantasy computer with interaction combinators as its base operations.

## Filling

Filling is a simple virtual machine that performs interaction combinator reduction.
This is the core that Bowl is based on.

Filling code is not meant to be written by hand.

## Bowl

Bowl will be a GC-less functional language that compiles to Filling. It should be a
simple layer of syntactic sugar on top of Filling.

---

## Random thoughts

A higher order lambda that clones its variables cannot be cloned. You win some, you lose some...

Goal: bootstrapping I guess?

Inspired by: HVM2, uxn
