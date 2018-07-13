
module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

-- data DataStore : Type where
--      MkData : (schema : Schema) -> (size : Nat) -> (items : Vect size (SchemaType schema)) -> DataStore

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)


-- data Command = Add String
--              | Search String
--              | Get Integer
--              | Size
--              | Quit

data Command : Schema -> Type where
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Quit : Command schema


addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) y = MkData schema _ (addToData store)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [y]
    addToData (x :: xs) = x :: addToData xs


sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp = let val = cast inp in
          if val < 0
             then Nothing
             else let newVal = tot + val in
                      Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

parseBySchema : (schema : Schema) -> (str : String) -> Maybe $ SchemaType schema

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str = case parseBySchema schema str of
                                     Nothing => Nothing
                                     Just r => Just (Add r)
parseCommand schema "get" val = if all isDigit (unpack val) then Just (Get (cast val)) else Nothing
-- parseCommand schema "search" str = Just (Search str)
-- parseCommand schema "size" "" = Just Size
parseCommand schema "quit" "" = Just Quit
parseCommand schema _ args = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (a, b) = display a ++ ", " ++ display b

getEntry : (ind : Integer) -> (ds : DataStore) -> Maybe (String, DataStore)
getEntry ind ds = let storeItems = items ds in
                      (case (integerToFin ind (size ds)) of
                            Nothing => Just ("Out of bounds \n", ds)
                            (Just id) => Just (display (index id storeItems) ++ "\n", ds))

-- searchForEntry : (str : String) -> (ds : DataStore) -> Maybe (String, DataStore)
-- searchForEntry "" ds = Just ("", ds)
-- searchForEntry str ds = let storeItems = items ds
--                             filteredPair = Data.Vect.filter (isInfixOf str) storeItems
--                             filteredItems = snd filteredPair
--                             resultString = foldl (\acc, x => acc ++ getIndex x storeItems ++ ": " ++ x ++ "\n") "" filteredItems in
--                             Just (resultString, ds)
--                         where getIndex : String -> Vect m String -> String
--                               getIndex x storeItems = fromMaybe "0" $ (map show (map finToNat $ elemIndex x storeItems))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds inp = case parse (schema ds) inp of
                           Nothing => Just ("Invalid command inputed\n", ds)
                           (Just x) => (case x of
                                             (Add item) => Just ("ID " ++ show (size ds) ++ "\n", addToStore ds item)
                                             (Get ind) => getEntry ind ds
                                             -- (Search str) => searchForEntry str ds
                                             -- Size => Just ("Size Of Datastore: " ++ show (size ds) ++ "\n", ds)
                                             Quit => Nothing)

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput

