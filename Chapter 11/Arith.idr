
module Arith

import Data.Primitives.Views

public export
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'
public export
arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where bound : Int -> Int
        bound x with (divides x 12)
          bound ((12 * div) + rem) | (DivBy prf) = rem + 1


quiz : Stream Int -> (score : Nat) -> IO ()
quiz (n1 :: n2 :: ns) score =
     do putStrLn $ "Thy score: " ++ show score
        putStr $ show n1 ++ " * " ++ show n2 ++ "? "
        answer <- getLine
        if cast answer == n1 * n2
           then putStrLn "Correct" >>= \_ => quiz ns $ score + 1
           else putStrLn ("Wrong. Answer is: " ++ show (n1 * n2)) >>= \_ => quiz ns score


-- 11.1 Ex. # 1
every_other : Stream ty -> Stream ty
every_other (v1 :: v2 :: vs) = v2 :: every_other vs

