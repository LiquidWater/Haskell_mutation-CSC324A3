{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..),
    runOp, (>>>), (>~>), returnVal,
    Value(..), StateOp(..),
    alloc, free
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
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Pointer a -> a -> StateOp a

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Integer -> a -> StateOp (Pointer a)

instance Mutable Value where
        get (P addr) = StateOp (\mem -> 
            if containsA mem addr
                then (lookupA mem addr, mem)
                else error "does not contain pointer! (get)")

        set (P addr) val = StateOp (\mem -> 
            if containsA mem addr
                then (val, updateA mem (addr, val))
                else error "does not contain pointer! (set)")

        def addr val = StateOp (\mem ->
            if containsA mem addr
                then error "pointer addr already defined!"
                else (P addr, insertA mem (addr, val)))

instance Mutable Integer where
        get (P addr) = StateOp (\mem -> 
            if containsA mem addr
                then let (IntVal result) = lookupA mem addr in (result, mem)
                else error "does not contain pointer! (get)")

        set (P addr) val = StateOp (\mem -> 
            if containsA mem addr
                then (val, updateA mem (addr, IntVal val))
                else error "does not contain pointer! (set)")

        def addr val = StateOp (\mem ->
            if containsA mem addr
                then error "pointer addr already defined!"
                else (P addr, insertA mem (addr, IntVal val)))

instance Mutable Bool where
        get (P addr) = StateOp (\mem -> 
            if containsA mem addr
                then let (BoolVal result) = lookupA mem addr in (result, mem)
                else error "does not contain pointer! (get)")

        set (P addr) val = StateOp (\mem -> 
            if containsA mem addr
                then (val, updateA mem (addr, BoolVal val))
                else error "does not contain pointer! (set)")

        def addr val = StateOp (\mem ->
            if containsA mem addr
                then error "pointer addr already defined!"
                else (P addr, insertA mem (addr, BoolVal val)))

-- Part 2: Chaining
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

{-Then function-}
(>>>) :: StateOp a -> StateOp b -> StateOp b
(>>>) f g = StateOp (\mem ->
    let (_, mem2) = runOp f mem
    in runOp g mem2)

{-Bind function-}
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
(>~>) f g = StateOp (\mem ->
    let (x, mem2) = runOp f mem
        newOp = g x
    in runOp newOp mem2)

{-
A function that takes a value, then creates a new StateOp which doesn't
interact with the memory at all, and instead just returns the value as
the first element in the tuple.
-}
returnVal :: a -> StateOp a
returnVal a = StateOp (\x -> (a, []))

-- Part 4 Safety Improvements

{-
Similar to def, except that the function automatically generates a fresh
(i.e., unused) number to bind in the value in memory, rather than accepting a
number as a parameter.
-}
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc val = undefined

{-
Takes a pointer, and removes the corresponding name-value binding from the
memory. You should add to AList.hs to do this.
-}
free :: Mutable a => Pointer a -> StateOp ()
free ptr = undefined

{-David's Test Code-}

f :: Integer -> StateOp Bool
f x =
    def 1 4 >~> \p1 ->
    def 2 True >~> \p2 ->
    set p1 (x + 5) >>>
    get p1 >~> \y ->
    set p2 (y > 3) >>>
    get p2

g :: Integer -> StateOp Integer
g x =
    def 1 (x + 4) >~> \p ->
    get p >~> \y ->
    returnVal (x * y)
