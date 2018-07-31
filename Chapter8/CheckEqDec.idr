
module CheckEqDec

data Vect : Nat -> (elem : Type) -> Type where
     Nil : Vect Z elem
     (::) : elem -> Vect n elem -> Vect (1 + n) elem

zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              (Yes prf) => Yes (cong prf)
                              (No contra) => No (noRec contra)


-- Ex 8.3 #1
headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

-- Ex 8.3 #2
DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                 (Yes prf1) => case decEq xs ys of
                                                   (Yes prf2) => Yes $ rewrite prf1 in rewrite prf2 in Refl
                                                   (No contra2) => No $ tailUnequal contra2
                                 (No contra1) => No $ headUnequal contra1
