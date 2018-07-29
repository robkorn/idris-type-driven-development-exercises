

public export
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                         Nothing => Nothing
                         Just prf => Just $ cong prf

-- Ex 8.1 #1
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf
-- Or alternative:
-- same_cons Refl = Refl

-- Ex 8.1 #2
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

-- Ex 8.1 #3
data ThreeEq : a -> b -> c -> Type where
     CreateThreeEq : ThreeEq z z z

-- Ex 8.1 #4
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z CreateThreeEq = CreateThreeEq
