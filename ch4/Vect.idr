import Data.Fin

data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs

index : (i : Fin n) -> Vect n elem -> elem
index FZ (x :: xs) = x
index (FS i) (x :: xs) = index i xs

tryIndex : {n: _} -> Integer -> Vect n a -> Maybe a
-- if integer can be converted to Fin of size n then apply index function
tryIndex i xs = map (\idx => index idx xs) $ integerToFin i n

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] _ = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

vectTake : (i : Fin (S n)) -> Vect n elem -> Vect (finToNat i) elem
vectTake FZ xs = []
vectTake (FS x) (y :: xs) = y :: vectTake x xs

sumEntries : Num a => {n : _} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries _ [] _ = ?sumEntries_missing_case_1
sumEntries pos xs ys = map (\idx => index idx xs + index idx ys) $ integerToFin pos n
