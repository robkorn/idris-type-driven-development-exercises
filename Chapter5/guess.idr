
import readNum

guess : (target : Nat) -> IO ()
guess target = do putStrLn "Guess a number"
                  Just g <- readNumber | Nothing => (putStrLn "Error: Only input digits" >>= \_ => guess target)
                  case compare g target of
                       LT => putStrLn "Too low!" >>= \_ => guess target
                       EQ => putStrLn "Got it!"
                       GT => putStrLn "Too high!" >>= \_ => guess target

