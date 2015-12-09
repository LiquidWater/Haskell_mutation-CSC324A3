{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module CompoundMutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..),  -- Part 0 and Part 1
    runOp, (>>>), (>~>), returnVal,  -- Part 2
    StateOp(..), alloc, free         -- Part 4
    )
    where

import AList (AList, lookupA, insertA, updateA, containsA, removeA)

-- Part 1: Starter Code
-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer |
                 PP Integer Integer  --person pointer has 2 addr. one per attr

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

{-Creating instances of the typeclasses-}

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

instance Mutable Person where       --Part 5 person functions
        --get the 2 parts separately then combine to ship
        get (PP aaddr iaddr) = StateOp (\mem ->
            let
                (age, _) = runOp (get (P aaddr)) mem 
                (iss, _) = runOp (get (P iaddr)) mem
                person = Person age iss
            in
                (person, mem))
        --set each part then build person to return
        set (PP aaddr iaddr) person = StateOp (\mem ->
            let
                Person age iss = person
                (_ , endm) =
                    runOp ((set (P aaddr) age) >>> (set (P iaddr) iss)) mem
            in
                (person, endm))
        {-alloc each slot and place age and is student in mem, return compoint
        pointer-}
        def addr person = StateOp (\mem ->
            let
                Person age iss = person
                (P p1, mem1) = runOp (alloc age)mem
                (P p2, mem2) = runOp (alloc iss)mem1
                per2 = PP p1 p2
            in
                (per2, mem2))

-- Part 2: Chaining
{-Functions and data provided by David-}
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

{-
  Then function
  Based off of StackOp's then function
-}
(>>>) :: StateOp a -> StateOp b -> StateOp b
(>>>) f g = StateOp (\mem ->
    let (_, mem2) = runOp f mem
    in runOp g mem2)

{-
  Bind function
  Based off of StackOp's bind function
-}
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
returnVal a = StateOp (\x -> (a, x))

-- Part 4 Safety Improvements
{-
  Similar to def, except that the function automatically generates a fresh
  (i.e., unused) number to bind in the value in memory, rather than accepting a
  number as a parameter.
-}
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc val = StateOp(\mem ->
    let 
       i = (take 1 (filter (\x -> not (containsA mem x)) [1..])) !! 0
       (_, endm) = runOp(def i val) mem
    in
        (P i, endm))

{-
  Takes a pointer, and removes the corresponding name-value binding from the
  memory. You should add to AList.hs to do this.
-}
free :: Mutable a => Pointer a -> StateOp ()
free (P ptr) = StateOp (\mem -> ((), removeA mem ptr))

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

--Part 5
-- A type representing a person with two attributes:
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving Show

--function to retrieve an attr pointer from a person
(@@) :: (Mutable a, Mutable b) => Pointer a -> (Pointer a -> Pointer b) -> Pointer b
(@@) point func = func point

--simple pattern matching to get the age pointer
age :: (Mutable a, Mutable b) => Pointer a -> Pointer b
age (PP x _) = P x

--simple pattern matching for the student pointer
isStudent:: (Mutable a, Mutable b) => Pointer a -> Pointer b
isStudent (PP _ x) = P x

personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x =
    -- not using alloc, but we could
    def 1 person >~> \personPointer ->
    get (personPointer @@ age) >~> \oldAge ->
    set (personPointer @@ age) x >>>
    get (personPointer @@ isStudent) >~> \stu ->
    get (personPointer @@ age) >~> \newAge ->
    set personPointer (Person (2 * newAge) (not stu)) >>>
    get personPointer >~> \newPerson ->
    get (personPointer @@ isStudent) >~> \newStu ->
    returnVal (oldAge, newStu, newPerson)