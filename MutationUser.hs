{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

module MutationUser (
    pointerTest, swap, swapCycle
    )
    where

import Mutation (
    get, set, def, Mutable, Memory, Pointer(..), Value(..), StateOp(..),
    (>>>), (>~>), runOp
    )

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> StateOp (Pointer Integer, Pointer Bool)
pointerTest int = StateOp (\mem ->
    let 
        mem_100 = IntVal (int + 3)
        mem_500 = BoolVal (int > 0)
        (_, newOpResult) = runOp ((def 100 mem_100) >>> (def 500 mem_500)) mem
    in 
    ((P 100, P 500), newOpResult))

-- Part 3 Calling with references

{-
Takes two pointers and swaps the values they refer to. Reminder that this is
not something we knew how to do otherwise in either Racket (without mutation)
or Haskell.
-}
swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap pointer1 pointer2 = undefined

{-
Takes a list of pointers p1, ..., pn, with corresponding values
v1, ..., vn, and sets p1's value to v2, p2's value to v3., etc., and
pn's value to v1. This function should not change anything if its argument
has length less than 2.
-}
swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle pointer_list = undefined
