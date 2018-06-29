
import Data.Vect


createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans
addMatrix : Num a => Vect m (Vect n a) -> Vect m (Vect n a) -> Vect m (Vect n a)
addMatrix [] [] = []
addMatrix xs ys = zipWith (zipWith (+)) xs ys




-- dotProdHelper : Num a => (xs : Vect n (Vect m a)) -> (ys : Vect p (Vect m a)) -> Vect n (Vect p a)
-- dotProdHelper [] [] = []
-- dotProdHelper [] (x :: xs) = ?dotProdHelper_rhs_4
-- -- dotProdHelper (x :: xs) ys = (foldl (\acc, row => acc ++ (sum $ Data.Vect.zipWith (*) x row)) [[]] ys) -- ++ dotProdHelper xs ys

-- dotProd : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
-- dotProd [] [] = []
-- dotProd [] (x :: xs) = []
-- dotProd xs ys = dotProdHelper xs (transposeMat ys)
