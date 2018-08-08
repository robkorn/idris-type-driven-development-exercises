
module SnocList

-- data SnocList ty = Empty | Snoc (SnocList ty) ty

data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])


snocListHelp : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp {input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in
                                              snocListHelp (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs


-- myReverseHelper : (input : List a) -> SnocList input -> List a
-- myReverseHelper [] Empty = []
-- myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

-- myReverse : List a -> List a
-- myReverse xs = myReverseHelper xs $ snocList xs

-- Alternate attempt to write a variation of myReverse that is also total before I was introduced to fun | sugaring.
-- myReverse : List a -> List a
-- myReverse xs = myRev xs $ snocList xs
--   where
--   myRev : (xs : List a) -> SnocList xs -> List a
--   myRev xs slxs with (slxs)
--     myRev [] slxs | Empty = []
--     myRev (ys ++ [x]) slxs | (Snoc rec) = x :: myRev ys rec


myReverse : List a -> List a
myReverse xs with (snocList xs)
  myReverse [] | Empty = []
  myReverse (ys ++ [x]) | (Snoc rec) = x :: myReverse ys | rec


isSuffix : Eq a => List a -> List a -> Bool
isSuffix xs ys with (snocList xs)
  isSuffix [] ys | Empty = True
  isSuffix (zs ++ [z]) ys | (Snoc zsrec) with (snocList ys)
    isSuffix (zs ++ [z]) [] | (Snoc zsrec) | Empty = False
    isSuffix (zs ++ [z]) (ys ++ [y]) | (Snoc zsrec) | (Snoc ysrec) = case z == y of
                                                                          False => False
                                                                          True => isSuffix zs ys | zsrec | ysrec


