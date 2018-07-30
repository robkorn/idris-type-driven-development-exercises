
module AppendVec

import Data.Vect

-- append : Vect n elem -> Vect m elem -> Vect (n + m) elem
-- append [] ys = ys
-- append (x :: xs) ys = x :: append xs ys

append_nil : (ys : Vect m elem) -> Vect (plus m 0) elem
append_nil {m} ys = rewrite plusZeroRightNeutral m in ys

append_xs : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
append_xs {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)

-- Ex 8.2 #1
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes j@(S k) m = let p = myPlusCommutes k m in
                           rewrite p in rewrite (plusSuccRightSucc m k) in Refl

-- Ex 8.2 #2

