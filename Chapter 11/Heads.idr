
module Heads

import Arith
import Data.Primitives.Views

data Face : Type where
     Heads : Face
     Tails : Face

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z (value :: xs) = []
coinFlips (S k) (value :: xs) = getFace value :: coinFlips k xs
  where getFace : Int -> Face
        getFace x with (divides x 2)
          getFace ((2 * div) + rem) | (DivBy prf) = if rem == 0 then Heads else Tails


