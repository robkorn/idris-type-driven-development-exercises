
module ArithCmdDo


data Input = Answer Int | QuitCmd

public export
data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String

     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

data Fuel = Empty | More (Lazy Fuel)

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand (f res)

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Empty y = pure Nothing
run (More x) (Quit y) = pure $ Just y
run (More x) (Do z f) = do res <- runCommand z
                           run x $ f res

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf $ ConsoleIO b) -> ConsoleIO b
  (>>=) = Do

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure $ Answer $ cast answer

mutual
  correct : Stream Int -> (score : Nat) -> (qNum : Nat) -> ConsoleIO Nat
  correct nums score qNum = do PutStr "Correct!\n"
                               quiz nums (score + 1) $ qNum + 1

  wrong : Stream Int -> Int -> (score : Nat) -> (qNum : Nat) -> ConsoleIO Nat
  wrong nums ans score qNum = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                                 quiz nums score $ qNum + 1

  -- Ex 11.3 # 1
  quiz : Stream Int -> (score : Nat) -> (qNum : Nat) -> ConsoleIO Nat
  quiz (n1 :: n2 :: ns) score qNum = do PutStr ("Score thus far: " ++ show score ++ " / " ++ show qNum ++ "\n")
                                        inp <- readInput (show n1 ++ " * " ++ show n2 ++ "? ")
                                        case inp of
                                              (Answer x) => if x == (n1 * n2)
                                                              then correct ns score qNum
                                                              else wrong ns (n1 * n2) score qNum
                                              QuitCmd => Quit score

startQuiz : IO (Maybe Nat)
startQuiz = run forever $ quiz [1..] 0 0
