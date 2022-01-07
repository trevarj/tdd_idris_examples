-- 3.3.3 examples

import Data.Vect

createEmpties : {n: _} -> Vect n (Vect 0 a)
createEmpties = replicate _ []

transposeHelper : (x : Vect n a)
               -> (xsTrans : Vect n (Vect k a))
               -> Vect n (Vect (S k) a)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : {n: _} -> Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in transposeHelper x xsTrans

transposeMat2 : {n: _} -> Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat2 [] = createEmpties
transposeMat2 (x :: xs) = let xsTrans = transposeMat2 xs in zipWith (::) x xsTrans

{-
Addition recursion

input: addMatrix [[1,2], [3,4]] [[5,6], [7,8]]
expected: [[6, 8], [10, 12]]

addMatrix (x :: xs) (y :: ys) = ([1,2] :: [[3,4]])  ([5,6] :: [[7,8]])
    addMatrix xs ys = [[3,4]] [[7,8]]
        addMatrix (x :: xs) (y :: ys) = ([3,4] :: [])  ([7,8] :: [])
            addMatrix [] ys = []
        sumMatrix: zipWith (+) [3,4] [7,8] :: [] ==> [[10,12]]
    sumMatrix: zipWith (+) [1,2] [3,4] :: [[10,12]] ==> [[6,8], [10,12]]
 -}
addMatrix : Num a => Vect n (Vect m a) 
                  -> Vect n (Vect m a) 
                  -> Vect n (Vect m a)
addMatrix [] ys = ys
addMatrix (x :: xs) (y :: ys) = let sumMatrix = addMatrix xs ys in zipWith (+) x y :: sumMatrix

multVect : {p: _} -> Num a 
        => Vect m a 
        -> Vect p (Vect m a) 
        -> Vect p a
multVect _ [] = []
multVect xs ys = map (\y => sum (zipWith (*) xs y)) ys

dotProduct : Num a => {n: _} -> {p: _} -> Vect n (Vect m a) 
         -> Vect p (Vect m a) 
         -> Vect n (Vect p a)
dotProduct [] _ = []
dotProduct _ [] = createEmpties
dotProduct (x :: xs) ys = let productMat = 
                             dotProduct xs ys 
                             in multVect x ys :: productMat

{- 
Transposes right matrix:
input: multMatrix [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]

transposeMat2 [[7,8,9,10], [11,12,13,14]] ==> [[7, 11], [8, 12], [9, 13], [10, 14]]

then performs matrix dot product
 -}
multMatrix : Num a => {n: _} -> {p: _} -> Vect n (Vect m a)
                             -> Vect m (Vect p a)
                             -> Vect n (Vect p a)
multMatrix xs ys = dotProduct xs (transposeMat2 ys)
