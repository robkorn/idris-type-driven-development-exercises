
module DescribeList

data ListLast : List a -> Type where
     Empty : ListLast []
     NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty ys y => NonEmpty (x :: ys) y

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty it is."
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Empty it is not, initial: " ++ show xs

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

describeListEWith : List Int -> String
describeListEWith xs with (listLast xs)
  describeListEWith [] | Empty = "Empty it is."
  describeListEWith (ys ++ [x]) | (NonEmpty ys x) =  "Empty it is not, initial: " ++ show ys


myReverse : List a -> List a
myReverse xs with (listLast xs)
  myReverse [] | Empty = []
  myReverse (ys ++ [x]) | (NonEmpty ys x) = x :: myReverse ys

