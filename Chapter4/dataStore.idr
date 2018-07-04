
module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command = Add String
             | Search String
             | Get Integer
             | Size
             | Quit

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

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = if all isDigit (unpack val) then Just (Get (cast val)) else Nothing
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ args = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (ind : Integer) -> (ds : DataStore) -> Maybe (String, DataStore)
getEntry ind ds = let storeItems = items ds in
                      (case (integerToFin ind (size ds)) of
                            Nothing => Just ("Out of bounds \n", ds)
                            (Just x) => Just (index x storeItems ++ "\n", ds))

searchForEntry : (str : String) -> (ds : DataStore) -> Maybe (String, DataStore)
searchForEntry "" ds = Just ("", ds)
searchForEntry str ds = let storeItems = items ds
                            filteredPair = Data.Vect.filter (isInfixOf str) storeItems
                            filteredItems = snd filteredPair
                            resultString = foldl (\acc, x => acc ++ getIndex x storeItems ++ ": " ++ x ++ "\n") "" filteredItems in
                            Just (resultString, ds)
                        where getIndex : String -> Vect m String -> String
                              getIndex x storeItems = fromMaybe "0" $ (map show (map finToNat $ elemIndex x storeItems))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds inp = case parse inp of
                           Nothing => Just ("Invalid command inputed\n", ds)
                           (Just x) => (case x of
                                             (Add item) => Just ("ID " ++ show (size ds) ++ "\n", addToStore ds item)
                                             (Get ind) => getEntry ind ds
                                             (Search str) => searchForEntry str ds
                                             Size => Just ("Size Of Datastore: " ++ show (size ds) ++ "\n", ds)
                                             Quit => Nothing)

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

