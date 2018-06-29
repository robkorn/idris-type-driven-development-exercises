
module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) y = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [y]
    addToData (x :: xs) = x :: addToData xs


sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp = let val = cast inp in
          if val < 0
             then Nothing
             else let newVal = tot + val in
                      Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

main : IO ()
main = replWith 0 "Value: " sumInputs

