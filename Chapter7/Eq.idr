
module Eq


data Matter : Type where
     Solid : Matter
     Liquid : Matter
     Gas : Matter

Eq Matter where
   (==) Solid Solid = True
   (==) Liquid Liquid = True
   (==) Gas Gas = True
   (==) _ _ = False
   (/=) x y = not (x == y)

occurances : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurances item [] = 0
occurances item (x :: xs) = case x == item of
                                 False => occurances item xs
                                 True => 1 + occurances item xs

