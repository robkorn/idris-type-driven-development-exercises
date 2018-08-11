
module Streams

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (label :: ls) (x :: xs) = (label, x) :: labelWith ls xs

label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)
