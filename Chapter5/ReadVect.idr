
module ReadVect

import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do s <- getLine
                       rest <- readVectLen k
                       pure (s :: rest)

-- Personal blind attempt #1 to build a readVectUnknownLen function using DPair
readVectUnknownLenAttempt1 : IO (l : Nat ** Vect l String)
readVectUnknownLenAttempt1 = do n <- putStrLn "How many vects?" >>= \_ => getLine
                                v <- readVectLen (cast n)
                                pure $ MkDPair (cast n) v

-- Personal blind attempt #2 to build a readVectUnknownLen function with tail recursion & DPairs
readVectUnknownLenAttempt2 : IO (l : Nat ** Vect l String)
readVectUnknownLenAttempt2 = go $ pure $ MkDPair 0 []
                            where go : IO (l : Nat ** Vect l String) -> IO (n : Nat ** Vect n String)
                                  go iocdp = do inp <- getLine
                                                cdp <- iocdp
                                                case inp /= "q" of
                                                    False => iocdp
                                                    True => go $ pure $ MkDPair (fst cdp + 1) ((snd cdp) ++ [inp])

