
import readNum
import System

guess : (iteration : Nat) -> (target : Nat) -> IO ()
guess iteration target = let itn = iteration + 1 in
                      do putStrLn $ "This is attempt number: " ++ (show itn)
                         putStrLn "Guess a number"
                         Just g <- readNumber | Nothing => (putStrLn "Error: Only input digits" >>= \_ => (guess itn target))
                         case compare g target of
                             LT => putStrLn "Too low!" >>= \_ => guess itn target
                             EQ => putStrLn "Got it!"
                             GT => putStrLn "Too high!" >>= \_ => guess itn target


main : IO ()
main = do n <- map ((-) 20000) $ time
          guess 0 $ cast n
