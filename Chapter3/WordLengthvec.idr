
import Data.Vect


allLengths : Vect l String -> Vect l Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs


insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs


insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted


myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

myRev : List a -> List a
myRev [] = []
myRev (x :: xs) = myRev xs ++ [x]

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: map f xs

vecMap : (a -> b) -> Vect n a -> Vect n b
vecMap f [] = []
vecMap f (x :: xs) = f x :: vecMap f xs
