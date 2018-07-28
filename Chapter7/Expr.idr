

module Expr

data Expr num = Val num
    | Add (Expr num) (Expr num)
    | Sub (Expr num) (Expr num)
    | Mul (Expr num) (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

-- Ex 7.2 #1
Cast ty String => Show (Expr ty) where
    show (Val x) = cast x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y

-- Ex 7.2 #2
(Neg num, Integral num, Eq num) => Eq (Expr num) where
    (==) x y = eval x == eval y

-- Ex 7.2 #3
(Neg num, Integral num) => Cast (Expr num) num where
  cast orig = eval orig

-- Ex 7.3 #1
Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)

-- Ex 7.3 #2
data MyVect : (size : Nat) -> (ty : Type) -> Type where
     Nil : MyVect Z ty
     (::) : ty -> MyVect size ty -> MyVect (S size) ty

Eq ty => Eq (MyVect n ty) where
  (==) x y = ?Eq_rhs_1
