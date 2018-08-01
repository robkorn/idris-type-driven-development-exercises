

module ElemType

data Vect : Nat -> (ty : Type) -> Type where
     Nil : Vect Z ty
     (::) : ty -> Vect n ty -> Vect (S n) ty

data Elem : a -> Vect k a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible


notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem value y -> Void) -> Elem value (x :: y) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: y) = case decEq value x of
                             (Yes Refl) => Yes Here
                             (No notHere) => case isElem value y of
                                                 (Yes prf) => Yes (There prf)
                                                 (No notThere) => No (notInTail notHere notThere)

-- Exercise 9.1 # 1
data LElem : a -> List a -> Type where
     LHere : LElem x (x :: xs)
     LThere : (later : LElem x xs) -> LElem x (y :: xs)

