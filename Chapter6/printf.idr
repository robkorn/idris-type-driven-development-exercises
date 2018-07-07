
module Printf

import Data.Vect

data Format = Number Format
            | Chr Format
            | Float Format
            | Str Format
            | Lit String Format
            | End

Matrix : Nat -> Nat -> Type
Matrix Z Z = Vect 0 (Vect 0 Nat)
Matrix Z h = Vect 0 (Vect h Nat)
Matrix h j = Vect h (Vect j Nat)

TupleVect : (size : Nat) -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

-- Ex. 1
testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

-- Ex. 3
test : TupleVect 4 Nat
test = (1,2,3,4,())

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Chr fmt) = (c : Char) -> PrintfType fmt
PrintfType (Float fmt) = (f : Double) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String


printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Float fmt) acc = \f => printfFmt fmt (acc ++ show f)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit str fmt) acc = printfFmt fmt (acc ++ str)
printfFmt End acc = acc


toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number $ toFormat chars
toFormat ('%' :: 'c' :: chars) = Chr $ toFormat chars
toFormat ('%' :: 'f' :: chars) = Float $ toFormat chars
toFormat ('%' :: 's' :: chars) = Str $ toFormat chars
toFormat ('%' :: chars) = Lit "%" $ toFormat chars
toFormat (c :: chars) = case toFormat chars of
                             Lit str chars' => Lit (strCons c str) chars'
                             fmt => Lit (strCons c "") fmt


printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
