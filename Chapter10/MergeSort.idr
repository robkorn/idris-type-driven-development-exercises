

module MergeSort

data SplitList : List a -> Type where
     SplitNil : SplitList []
     SplitOne : SplitList [x]
     SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

total
splitList : (l : List a) -> SplitList l
splitList inp = splitListHelp inp inp
  where
    splitListHelp : List a -> (l : List a) -> SplitList l
    splitListHelp _ [] = SplitNil
    splitListHelp _ [x] = SplitOne
    splitListHelp (_ :: _ :: counter) (i :: items) = case splitListHelp counter items of
                                                          SplitNil => SplitOne
                                                          SplitOne {x} => SplitPair [i] [x]
                                                          (SplitPair lefts rights) => SplitPair (i :: lefts) rights
    splitListHelp _ items = SplitPair [] items


mergeSort : Ord a => List a -> List a
mergeSort xs with (splitList xs)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts) (mergeSort rights)
