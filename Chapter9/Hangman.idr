
module Hangman

import Data.Vect

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) -> (missing : Vect letters Char) -> WordState guesses_remaining letters

data Finished : Type where
     Lost : (game : WordState 0 (S letters)) -> Finished
     Won : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]


isValidInut : (cs : List Char) -> Dec (ValidInput cs)
isValidInut [] = No $ ?isValidInut_rhs_3
isValidInut (x :: xs) = ?isValidInut_rhs_2

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = ?ivs

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Enter a guess: "
               a <- getLine
               case isValidString a of
                    (Yes prf) => pure (_ ** prf)
                    (No contra) => putStrLn "Please enter a single character" >>= \_ => readGuess

game : WordState (S guesses) (S letters) -> IO Finished
game st = ?game_rhs

