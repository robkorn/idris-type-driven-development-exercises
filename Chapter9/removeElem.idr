
module RemoveElem

import Data.Vect


-- Dependent pair implementation of removeElem for fun
depPRemElem : DecEq a => (value : a) -> (xs : Vect n a) -> (len ** Vect len a)
depPRemElem value [] = (_ ** [])
depPRemElem value (x :: xs) = case decEq value x of
                                   (Yes prf) => (_ ** xs)
                                   (No contra) => (_ ** (x :: (snd $ depPRemElem value xs)))

maryInVector : Elem "Mary" ["Pete", "Bob", "Mary"]
maryInVector = There (There Here)

removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
removeElem value (value :: ys) Here = ys
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) = y :: (removeElem value ys later)

removeElem_auto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf

public export
removeElemAuto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElemAuto value (value :: ys) {prf = Here} = ys
removeElemAuto {n = Z} value (y :: []) {prf = (There later)} = absurd later
removeElemAuto {n = (S k)} value (y :: ys) {prf = (There later)} = y :: (removeElemAuto value ys)
