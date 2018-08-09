
module MergeSortView

import Data.List.Views
import Data.Nat.Views
import Data.Vect
import Data.Vect.Views

mergeSort : Ord a => List a -> List a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (mergeSort lefts | lrec) (mergeSort rights | rrec)


-- 10.2 Ex # 1
total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc rec) with (snocList ys)
    equalSuffix (zs ++ [x]) [] | (Snoc rec) | Empty = []
    equalSuffix (zs ++ [x]) (xs ++ [y]) | (Snoc zsrec) | (Snoc xsrec) = case x == y of
                                                                       False => []
                                                                       True => equalSuffix zs xs | zsrec | xsrec ++ [x]

-- 10.2 Ex # 2
total
vmergeSort : Ord a => Vect n a -> Vect n a
vmergeSort xs with (splitRec xs)
  vmergeSort [] | SplitRecNil = []
  vmergeSort [x] | SplitRecOne = [x]
  vmergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (vmergeSort lefts | lrec) (vmergeSort rights | rrec)

-- 10.2 Ex # 3
total
toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = toBinary n | rec ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = toBinary n | rec ++ "1"

-- 10.2 Ex # 4
total
palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = case x == y of
                                                     False => False
                                                     True => palindrome ys | rec

