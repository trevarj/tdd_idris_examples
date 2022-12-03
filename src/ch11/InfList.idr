import Data.Stream

data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem
%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1)) -- can leave out Delay, see below

getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix 0 xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs -- implicitly applies `Force xs`


