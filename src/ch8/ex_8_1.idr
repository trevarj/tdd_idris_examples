{-
Implement the following function, which states that if you add the same value onto the front of
equal lists, the resulting lists are also equal: 
 -}
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = cong (x ::) prf

{-
Because this function represents an equality proof, it’s sufficient to know that your definition type-checks and is total:

*ex_8_1> :total same_cons
Main.same_cons is Total
 -}

same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl prf = cong (x ::) prf

data ThreeEq : a -> b -> c -> Type where
     Same : (a : ty)  -> ThreeEq a a a

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x (Same x) = Same (S x)
