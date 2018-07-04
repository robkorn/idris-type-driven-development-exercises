
module Main

import System

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Kaboom"
countdown secs@(S k) = do putStrLn $ show secs
                          usleep 1000000
                          countdown k
