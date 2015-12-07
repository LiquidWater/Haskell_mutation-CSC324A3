{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

import Mutation (
    get, set, def, Mutable, Memory, Pointer(..), Value(..)
    )

--data Pointer a = P Integer

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
pointerTest int mem = let
    mem_100 = IntVal (int + 3)
    mem_500 = BoolVal (int > 0)
    (np_100, nmem_100) = def mem 100 mem_100 
    (np_500, nmem_500) = def nmem_100 500 mem_500 in
    ((P 100, P 500), nmem_500)

