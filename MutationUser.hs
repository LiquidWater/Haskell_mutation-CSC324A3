{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

module MutationUser (
    pointerTest, swap, swapCycle
    )
    where

import Mutation (
    get, set, def, Mutable, Memory, Pointer(..), Value(..), StateOp(..),
    (>>>), (>~>), runOp, alloc, free, returnVal
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
swap (P pointer1) (P pointer2) = StateOp (\mem ->
    let
        p1 = (P pointer1)
        p2 = (P pointer2)
        (value1, _) = runOp (get (p1 :: Pointer Value)) mem
        (value2, _) = runOp (get (p2 :: Pointer Value)) mem

        firstOp = free (p1 :: Pointer Value)
        secOp = free (p2 :: Pointer Value)
        thirdOp = def pointer1 value2
        fourthOp = def pointer2 value1

        firstSet = firstOp >>> secOp
        secSet = thirdOp >>> fourthOp
    in
        runOp (firstSet >>> secSet >>> returnVal ()) mem
    )

{-
Takes a list of pointers p1, ..., pn, with corresponding values
v1, ..., vn, and sets p1's value to v2, p2's value to v3., etc., and
pn's value to v1. This function should not change anything if its argument
has length less than 2.
-}
swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle pointer_list =  StateOp (\mem ->
    let
        if (length pointer_list < 2) -- if not large enough do nothing and return
            then
                returnVal = ((), mem)

            else 
                x:xs = pointer_list
                y = xs !! 0
                _ = (swap x y) mem --swap current position and next so base case has no work, v2 moved up with each recurse
                returnVal = swapCycle(y:xs) mem
    in
    returnVal)

