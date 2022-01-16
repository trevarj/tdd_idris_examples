
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs

-- Exercise 7.3.1

Functor Expr where
    map func (Val x) = Val (func x)
    map func (Add x y) = Add (map func x) (map func y)
    map func (Sub x y) = Sub (map func x) (map func y)
    map func (Mul x y) = Mul (map func x) (map func y)
    map func (Div x y) = Div (map func x) (map func y)
    map func (Abs x) = Abs (map func x)

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Show num => Show (Expr num) where 
     show (Val x) = show x
     show (Add x y) = show x ++ " + " ++ show y 
     show (Sub x y) = show x ++ " - " ++ show y 
     show (Mul x y) = show x ++ " * " ++ show y 
     show (Div x y) = show x ++ " / " ++ show y 
     show (Abs x) = "|" ++ show x ++ "|"

(Eq num, Num num, Abs num, Neg num, Integral num) => Eq (Expr num) where
     (==) x y = (eval x) == (eval y)

(Cast num toType, Abs num, Neg num, Integral num) => Cast (Expr num) toType where
     cast expr = cast (eval expr)