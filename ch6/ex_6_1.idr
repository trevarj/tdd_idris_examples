import Data.Vect

{- 
Matrix type using type synonyms
 -}

Matrix : Nat -> Nat -> Type
Matrix i j = Vect i (Vect j Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]