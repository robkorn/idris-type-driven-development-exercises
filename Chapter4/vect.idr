
data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] ys = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys



-- Random implicit argument messing around
blah : Vect n a -> Nat
blah {n} _ = n + 5

blah2 : Vect n a -> Type
blah2 {a} _ = a

a : Nat
a = blah [1,2,3]

b : Type
b = blah2 [1,2,3]
