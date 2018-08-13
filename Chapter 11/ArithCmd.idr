
module ArithCmd

import System
import Arith

%default total

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

data Fuel = Empty | More (Lazy Fuel)

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Empty y = pure Nothing
run (More x) (Quit y) = pure $ Just y
run (More x) (Do z f) = do res <- runCommand z
                           run x $ f res
mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score
    = do PutStr "Correct!\n"
         quiz nums (score + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans score
    = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
         quiz nums score

  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (n1 :: n2 :: nums) score =
    do PutStr $ "Score so far: " ++ show score ++ "\n"
       PutStr $ show n1 ++ " * " ++ show n2 ++ "? "
       answer <- GetLine
       if toLower answer == "quit" then Quit score else
           if (cast answer == n1 * n2)
             then correct nums score
             else wrong nums (n1 * n2) score

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
               | Nothing => putStrLn "Ran out of fuel."
          putStrLn ("Final score: " ++ show score)
