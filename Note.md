I was thinking that it would be a good idea to \"parametrize\" each
algorithm by its own data strucutre.

For instance, the brute force algorithm seem to work better if we
represent the input formula as an array of arrays, whereas the DPLL
algorithm seem to be more optimized if each clause is represented by a
set and the formula is an array of Sets (since it require more
operations that are set-like in the clauses). Of course this is just a
guess, and I would like to benchmark them to check this guess. I did not
want to write one implementation for each algorithm using each possible
data structure. In order to avoid that, I would like to make the
algorithms polymorphic over the underlying data structure. The way to do
that in OCaml seems to be through `Functors`. A functor is just a module
that is parametrized by another module. In our case, we would define a
module type representing the abstract notion of a formula. This module
type would have to contain a set of operations that are powerful enough
to implement any operation we would like to do with a formula in any of
the algorithms we plan to implement. Then, the modules for the
algorithms would be parametrized by a `Formula` module type. This looks
like a good idea, since if we are successful in this, we could generate
a version of any algorithm with a new underlying data structure simply
by instantiating the functor with the corresponding module.

The problem with that is that I believe it is hard to come up with a
good set of operations to perform on formulas for our `Formula` module
type that would cover everything necessary. For instance, the DPLL rely
on a series of set-like operations. If we would add all of them to
`Formula`, we would bloat the module, and every new data structure we
would want to add will have to also implement these operations
(sometimes in complicated ways, if it is a data structure that
inherently does not support anything similar to set-like operations).
The alternative would be to keep `Formula` simple, with just high level
operations (e.g. remove literal from clause, instead of do the set
difference between these too clauses). This of course has the
disadvantage that we would have to implement a lot of things inside the
algorithm itself using these high level operations, which would
compromise its efficiency.

Maybe the first approach is still doable, if we spend more time on it. I
don\'t wanna do it now. For now I will fix the data structure of each
algorithm and implement a function to cast the data structure returned
by the parser to the data structure of the algorithm in each module.

- Tomaz
