
module TypeFuns

SorI : Bool -> Type
SorI False = String
SorI True = Int

getStringOrInt : (isInt : Bool) -> SorI isInt
getStringOrInt False = "Twenty four"
getStringOrInt True = 24

valToString : (isInt : Bool) -> SorI isInt -> String
valToString False x = trim x
valToString True x = cast x

vts : (isInt : Bool) -> (case isInt of
                              False => String
                              True => Int) -> String
vts False x = trim x
vts True x = cast x
