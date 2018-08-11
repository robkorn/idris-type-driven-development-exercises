
module squareRoot

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx =
  let next = (approx + (number / approx)) / 2 in
      next :: square_root_approx number next

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: vs) = value
square_root_bound (S k) number bound (v :: vs) =
  case (v * v) < bound of
       False => square_root_bound k number bound vs
       True => v

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.0000000001 (square_root_approx number number)
