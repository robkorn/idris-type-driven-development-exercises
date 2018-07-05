
module ReadVect

import Data.Vect

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do s <- getLine
                       rest <- readVectLen k
                       pure (s :: rest)

-- Personal blind attempt #1 to build a readVectUnknownLen function using DPair
readVectUnknownLenAttempt1 : IO (l : Nat ** Vect l String)
readVectUnknownLenAttempt1 = do n <- putStrLn "How many vects?" >>= \_ => getLine
                                v <- readVectLen (cast n)
                                pure $ ((cast n) ** v)

-- Personal blind attempt #2 to build a readVectUnknownLen function with tail recursion & DPairs
-- Sucess, though a little verbose
readVectUnknownLenAttempt2 : IO (l : Nat ** Vect l String)
readVectUnknownLenAttempt2 = go $ pure $ MkDPair _ []
                            where go : IO (l : Nat ** Vect l String) -> IO (n : Nat ** Vect n String)
                                  go iocdp = do inp <- getLine
                                                cdp <- iocdp
                                                case inp /= "" of
                                                    False => iocdp
                                                    True => go $ pure $ MkDPair _ ((snd cdp) ++ [inp])

readVect : IO (VectUnknown String)
readVect = do inp <- getLine
              case inp /= "" of
                   False => pure (MkVect _ [])
                   True => do MkVect _ xs <- readVect
                              pure $ MkVect _ (inp :: xs)

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect l xs) = putStrLn (show xs ++ " (length " ++ show l ++ ")")


readVectUnknownLen : IO (len ** Vect len String)
readVectUnknownLen = do inp <- getLine
                        if (inp == "")
                           then pure (_ ** [])
                           else do (_ ** xs) <- readVectUnknownLen
                                   pure (_ ** inp :: xs)


zipInputs : IO ()
zipInputs = do putStrLn "Input first vector: "
               (l1 ** v1) <- readVectUnknownLen
               putStrLn "Input second vector: "
               (l2 ** v2) <- readVectUnknownLen
               case exactLength l1 v2 of
                    Nothing => putStrLn "Error: The two vectors are different sizes"
                    (Just cv2) => putStrLn $ show $ Data.Vect.zip v1 cv2

readToBlank : IO (List String)
readToBlank = do inp <- getLine
                 case inp /= "" of
                      False => pure []
                      True => do sl <- readToBlank
                                 pure (inp :: sl)

readAndSave : IO ()
readAndSave = do inps <- concat <$> readToBlank
                 writeFile "ras.txt" inps
                 pure ()

parseFile : (r : File) -> IO (n : Nat ** Vect n String)
parseFile f = do isEOF <- fEOF f
                 case isEOF of
                      True => pure $ MkDPair _ []
                      False => do Right l <- fGetLine f | Left err => pure (_ ** [])
                                  MkDPair _ xs <- parseFile f
                                  pure (MkDPair _ (l :: xs))



readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right f <- openFile "ras.txt" Read | Left err => pure (MkDPair _ [])
                           parseFile f
