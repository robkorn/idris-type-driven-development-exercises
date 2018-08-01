
module RemoveElem

import Data.Vect


-- Dependent pair implementation of removeElem for fun
depPRemElem : DecEq a => (value : a) -> (xs : Vect n a) -> (len ** Vect len a)
depPRemElem value [] = (_ ** [])
depPRemElem value (x :: xs) = case decEq value x of
                                   (Yes prf) => (_ ** xs)
                                   (No contra) => (_ ** (x :: (snd $ depPRemElem value xs)))

-- removeElem : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
-- removeElem value (x :: xs) = case decEq value x of
--                                   (Yes prf) => xs
--                                   (No contra) => ?removeElem_rhs_3
