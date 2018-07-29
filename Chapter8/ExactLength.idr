
module ExactLength

import EqNat

data Vect : Nat -> Type -> Type where
     Nil : Vect Z ty
     (::) : ty -> Vect n ty -> Vect (S n) ty

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 Just (Same len) => Just input
