
module Foldable

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

totLen : List String -> Nat
totLen xs = foldr (\str, acc => acc + length str) 0 xs

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node l e r)
        = let leftFold = foldr func acc l
              rightFold = foldr func leftFold r in
              func e rightFold
