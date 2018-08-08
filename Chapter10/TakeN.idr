
module TakeN

data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

-- Ex. 10.1 # 1
total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) = case takeN k xs of
                             Fewer => Fewer
                             (Exact n_xs) => Exact $ x :: n_xs

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

-- Ex. 10.1 # 2
halves : List a -> (List a, List a)
halves xs = splitHalf (div (length xs) 2) xs
  where
  splitHalf : Nat -> List a -> (List a, List a)
  splitHalf n xs with (takeN n xs)
    splitHalf n xs | Fewer = ([], xs)
    splitHalf n (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)

