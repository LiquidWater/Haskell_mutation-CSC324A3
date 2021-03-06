{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA,
    containsA,
    removeA
    )
    where


type AList a b = [(a, b)]

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key =
    let
        (testkey, testitem) = head alist
    in
        if key == testkey
            then testitem
            else (lookupA (drop 1 alist) key)

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) =
    let
        templist = filter (\(a, b) -> a == key) alist
    in
        if null templist
            then alist ++ [(key,val)]
            else alist

-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA alist (key, val) =
    let templist = filter (\(a, b) -> a == key) alist
        --Removes the other key value pair if it does exist
        otherlist = filter (\(a, b) -> a /= key) alist
    in
        if null templist
            then alist
            else otherlist ++ [(key, val)]

{-Checks whether or not a key exists in the AList-}
containsA :: Eq a => AList a b -> a -> Bool
containsA alist key = any ((==) key) (getKeys alist)

{-Internal helper function to get all the keys in the AList-}
getKeys :: AList a b -> [a]
getKeys alist = map (\ (key, val) -> key) alist

{-Removes any element with the specified key from the AList-}
removeA :: Eq a => AList a b -> a -> AList a b
removeA alist key = filter (\(a, b) -> a /= key) alist

