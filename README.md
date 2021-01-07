# brute_force

Utilities to help one declare and check properties of finite types.

Generate all instances of a structure up to a particular size to gain intuition for what small
examples look like (e.g. the smallest adjunctions).

Examples:

"For all groups G (up to order n), and for all x and y in G, is it the case that (xy)^-1 = y^-1x^-1?"

"Show me (and store in a database) the 5 smallest natural transformations"

While generate-and-test is slow to check in a brute force style, it can be sped up by precomputation: separating the generating and test steps.

## Generation

The language being used is the calculus of inductive constructions https://hal.inria.fr/hal-01094195/document

(from nlab) Terms of a pure type system can be:

- Variables or constants
- abstraction (`λx: A, B`)
- dependent product (`Πt: A, B`)
- application (`f(x)`)

Each table has a hidden primary key that is computed as a hash of the unique information.

the natural numbers (`Nat`...up to n) ARE a database table (`val` attribute, unique constraint on `(val)`) where each row is an instance.

pairs of natural numbers are a database table (FKs `fst` and `snd` to `Nat` and unique constraint on `(fst, snd)`), where each row is an instance.

The type `Nat -> Nat` is a database table (`ID`, FK `dom` to `Nat`, FK `codom` to `Nat`, unique constraint on `(ID, dom)`) representing unary functions on `Nat` where each subset of rows with a given ID is an instance.

The type `Nat x Nat -> Nat` is a database table (`ID`, FK `dom` to `ID` of `Nat x Nat` and FK `codom` to `Nat`, unique constraint on `(ID, dom)`) representing binary functions on `Nat` where each subset of rows with a given ID is an instance.

Groups (up to group isomorphism) ARE a database table

- columns are:
  - FK to `Nat` (carrier type)
  - FK `mul` to `ID` of `Nat x Nat -> Nat`
  - FK `unit` to `Nat`
  - Furthermore, the group axioms need to hold: - `forall x y in G: `
