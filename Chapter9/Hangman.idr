
module Hangman

import Data.Vect
import removeElem

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) -> (missing : Vect letters Char) -> WordState guesses_remaining letters

data Finished : Type where
     Lost : (game : WordState 0 (S letters)) -> Finished
     Won : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]


validNil : ValidInput [] -> Void
validNil (Letter _) impossible

validNotSingle : ValidInput (x :: (y :: xs)) -> Void
validNotSingle (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No $ validNil
isValidInput (x :: []) = Yes $ Letter x
isValidInput (x :: (y :: xs)) = No validNotSingle

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (g ** ValidInput g)
readGuess = do putStr "Enter a guess: "
               a <- getLine
               case isValidString a of
                    (Yes prf) => pure (_ ** prf)
                    (No contra) => putStrLn "Please enter a single character" >>= \_ => readGuess

processGuess : (letter : Char) -> WordState (S guesses) (S letters) -> Either (WordState guesses (S letters)) (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      Yes prf => Right $ MkWordState word $ removeElemAuto letter missing
                                                      No contra => Left $ MkWordState word missing

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter c) <- readGuess
                                 case (processGuess c st) of
                                         (Left l) => do putStrLn "Your guess was wrong."
                                                        case guesses of
                                                            Z => pure $ Lost l
                                                            (S k) => game l
                                         (Right r) => do putStrLn "Correct!"
                                                         case letters of
                                                              Z => pure $ Won r
                                                              (S k) => game r

main : IO ()
main = let guessWord = "lambda"
           guessLetters = fromList (unpack guessWord)
           numGuesses = 3 in
        do gameres <- game {guesses=numGuesses-1} (MkWordState guessWord guessLetters)
           case gameres of
                (Lost game) => putStrLn $ "Thou hast lost. The word was: " ++ guessWord
                (Won game) => putStrLn "Congrats, you win!"
