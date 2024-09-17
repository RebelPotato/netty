# Wonton

Wonton will be a fantasy computer with interaction combinators as its base operations.

## Filling VM

Filling is a simple virtual machine that performs interaction combinator reduction.
This is the core that Bowl is based on.

Filling code can written by hand, but it is quite tedious. You can also draw filling code.

## Bowl

Bowl will be a GC-less functional language that compiles to Filling VM code.
It should be a simple layer of syntactic sugar on top of Filling.

## Soup

Soup will be a GC-less functional language that compiles to Bowl. It is a full featured
functional programming language that compiles to Bowl.

Boatloads of syntactic sugar.

Potential features:

* ADTs (as numbers and lambdas)
* typeclasses (without types? how?)
* delimited continuations (for free with open lambdas)

---

Inspired by: HVM2, uxn
