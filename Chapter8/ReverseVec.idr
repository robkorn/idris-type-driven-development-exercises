
module ReverseVec

import Data.Vect


myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} res = rewrite sym (plusCommutative k 1) in res
    -- Used sym for fun to get the hang of reversing direction of rewrites
