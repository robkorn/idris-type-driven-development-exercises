
module InfIO


data InfIO : Type where
     Do : IO a
          -> (a -> Inf InfIO)
          -> InfIO

data Fuel = Dry | More (Lazy Fuel)

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More $ tank k

run : Fuel -> InfIO -> IO ()
run Dry (Do action cont) = putStrLn "Out of fuel."
run (More x) (Do action cont) = action >>= \res => run x $ cont res

loopPrint : String -> InfIO
loopPrint m = Do (putStrLn m) (\_ => loopPrint m)

lp : String -> InfIO
lp m = do putStrLn m
          lp m
