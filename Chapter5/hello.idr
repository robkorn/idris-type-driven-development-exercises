
module Main

main : IO ()
main = do
     putStr "Enter thy name: "
     x <- getLine
     putStrLn ("Howdy " ++ x)


printLen : IO ()
printLen = getLine >>= putStr . show . length


printLonger : IO ()
printLonger = do
            putStr "First string: "
            a <- getLine
            putStr "2nd string: "
            b <- getLine
            print $ if length a > length b then length a else length b

pL : IO ()
pL = putStr "1st: " >>= \_ => getLine >>= \a => putStr "2nd: " >>= \_ => getLine >>= \b => print $ if length a > length b then length a else length b
