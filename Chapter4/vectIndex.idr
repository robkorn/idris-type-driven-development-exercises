
import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         (Just idx) => Just (index idx xs)


vectTake : (num : Fin (S n)) -> Vect n a -> Vect (finToNat num) a
vectTake FZ _ = []
vectTake (FS y) (x :: xs) = x :: vectTake y xs
