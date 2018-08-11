
module InfList


data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem
%name InfList xs, ys, za

-- 11.1 Ex. # 2
Functor InfList where
  map func (value :: xs) = func value :: map func xs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x+1))

getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

