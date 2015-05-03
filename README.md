Compiled Expressions
====================

This library can compile some Haskell functions to LLVM which can then be
used in Haskell in a way that is almost identical to the original form of
the Haskell function.

The class of functions that can be compiled this way are those that admit a
Traversal value (which includes all Traversable containers) containing
Floating -typeclassed numbers. This library uses a observable sharing and a
special numerical type to capture the structure of the computation and then
generates a LLVM IR that implements the function.

The library is highly experimental. Very large computations can take a long
time to compile. Some functions become much slower because GHC already has
optimized the Haskell code well. I've seen observable sharing fail on a
seemingly simple computation causing an explosion of code in LLVM side.
However, when it works, the performance boost can be astounding. In my
simulations of Hopfield neural networks, the compiled expression executes
around 30x faster. This package contains some cabal benchmarks.

Example usage
-------------

    -- This function compiles a function that computes the sum
    -- of 10 values at run-time.

    import Data.CompiledExpression

    -- describes the structure of the container we use
    list_structure :: [()]
    list_structure = replicate 10 ()

    -- make a compiled function
    compiled :: [Double] -> Double
    compiled = compileExpression1 traverse sum list_structure

    -- now 'compiled' works like 'sum', as long as lists of size 10 are used
    compiled [1,2,3,4,5,6,7,8,9,10] --> 55
    sum [1,2,3,4,5,6,7,8,9,10]      --> 55

How to report bugs, ask questions
---------------------------------

You can open a GitHub issue or e-mail me to <mikjuo@gmail.com>.

