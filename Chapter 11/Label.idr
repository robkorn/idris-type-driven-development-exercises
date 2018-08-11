
module Label

labelFrom : Integer -> List a -> List (Integer, a)
labelFrom n [] = []
labelFrom n (y :: ys) = (n, y) :: labelFrom (n+1) ys

label : List a -> List (Integer, a)
label = labelFrom 0
