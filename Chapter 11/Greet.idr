
module Greet

%default total

data InfIO : Type where
     Do : IO a ->
          (a -> Inf InfIO) ->
          InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

greet : InfIO
greet = do putStr "Enter thy name: "
           name <- getLine
           putStrLn ("Howdy " ++ name)
           greet
