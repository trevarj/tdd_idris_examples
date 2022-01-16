
-- 7.3 Exercises

data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs

Eq a => Eq (Vect n a) where
     (==) [] [] = True
     (==) (x :: xs) (y :: ys) = x == y && xs == ys
     (==) _ _ = False

Foldable (Vect n) where
     foldr func init [] = init
     foldr func init (x :: xs) = let acc = func x init in foldr func acc xs

Functor (Vect n) where
     map func [] = []
     map func (x :: xs) = func x :: map func xs
