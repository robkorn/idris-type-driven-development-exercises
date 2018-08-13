
module RunIO

%default total

data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

data Fuel = Dry | More (Lazy Fuel)

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

run : Fuel -> RunIO a -> IO (Maybe a)
run f (Quit v) = pure $ Just v
run Dry p = pure Nothing
run (More x) (Do a f) = do res <- a
                           run x $ f res

greet : RunIO ()
greet = do putStr "Thy name: "
           name <- getLine
           case name == "" of
                False => putStrLn ("Hello " ++ show name) >>= \_ => greet
                True => putStrLn "Ciao" >>= \_ => Quit ()

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do run forever greet
          pure ()
