-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

module NinetyNineProblems where

-- Problem 1 
-- Find the last element of a list.    
myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_ : xs) = myLast xs


-- Problem 2 
-- Find the last but one element of a list. 

myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1. 
elementAt :: [a] -> Int -> a
elementAt (x : _) 1 = x
elementAt (_ : xs) n = elementAt xs (n - 1)

elementAt2 :: [a] -> Int -> a
elementAt2 list i    = list !! (i-1)