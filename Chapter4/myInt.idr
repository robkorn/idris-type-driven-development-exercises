
data Expr = Val Int
          | Mult Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
%name Expr expr1, expr2

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Mult expr1 expr2) = evaluate expr1 * evaluate expr2
evaluate (Add expr1 expr2) = evaluate expr1 + evaluate expr2
evaluate (Sub expr1 expr2) = evaluate expr1 - evaluate expr2
