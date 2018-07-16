
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
     SetSchema : (newschema : Schema) -> Command schema
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

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString inp = getQuoted (unpack inp)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt inp = case span isDigit inp of
                            ("", rest) => Nothing
                            (num, rest) => Just (cast num, ltrim rest)
parsePrefix (x .+. y) inp = do l <- parsePrefix x inp
                               r <- parsePrefix y $ snd l
                               pure ((fst l, fst r), snd r)
-- parsePrefix (x .+. y) inp = case parsePrefix x inp of
--                                   Nothing => Nothing
--                                   Just (lval, inp') => case parsePrefix y inp' of
--                                                            Nothing => Nothing
--                                                            Just (rval, inp'') => Just ((lval, rval), inp'')

parseBySchema : (schema : Schema) -> (str : String) -> Maybe $ SchemaType schema
parseBySchema schema inp = case parsePrefix schema inp of
                                -- Just (res,  "") => Just res
                                Just _ => Nothing
                                Nothing => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str = case parseBySchema schema str of
                                     Nothing => Nothing
                                     Just r => Just (Add r)
parseCommand schema "get" val = if all isDigit (unpack val) then Just (Get (cast val)) else Nothing
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

