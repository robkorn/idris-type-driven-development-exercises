
module ArithTotal

import Data.Primitives.Views
import System

%default total

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run Dry y = putStrLn "Out of fuel."
run (More x) (Do y f) = do res <- y
                           run x (f res)

quiz : Stream Int -> (score : Nat) -> InfIO
quiz (v1 :: v2 :: vs) score =
  do putStrLn $ "Score so far: " ++ show score
     putStr $ show v1 ++ " * " ++ show v2 ++ "? "
     answer <- getLine
     case cast answer == v1 * v2 of
          False => do putStrLn $ "Incorrect. Answer is: " ++ show (v1 * v2)
                      quiz vs score
          True => do putStrLn $ "Correct!"
                     quiz vs (score + 1)

randoms : Int -> Stream Int
randoms seed = let seed' = 163525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do seed <- time
          run forever $ quiz (arithInputs $ fromInteger seed) 0


-- 11.2 Exercise
totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do putStrLn prompt
                             res <- getLine
                             putStrLn $ action res
                             totalREPL prompt action
