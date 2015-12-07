{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer,
    runOp, (>>>), (>~>),
    Value, StateOp, returnVal
    )
    where

import AList (AList, lookupA, insertA, updateA, containsA)

-- Part 1: Starter Code
-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: AList Integer a -> Pointer a -> a
    get mem (P addr) = lookupA mem addr

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: AList Integer a -> Pointer a -> a -> AList Integer a
    set mem (P addr) val = updateA mem (addr, val)

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: AList Integer a -> Integer -> a -> (Pointer a, AList Integer a)
    def mem addr val = 
        if containsA mem addr
            then error "addr already defined"
            else (ptr, modMem)
        where modMem = insertA mem (addr, val)
              ptr = (P addr) 


-- Part 2: Chaining
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp op mem = undefined

(>>>) :: StateOp a -> StateOp b -> StateOp b
(>>>) = undefined

(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
(>~>) = undefined

