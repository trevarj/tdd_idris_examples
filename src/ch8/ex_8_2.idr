import Data.Nat
import Data.Vect

{-
Exercise 1

Using plusZeroRightNeutral and plusSuccRightSucc, write your own version of plusCommutes:

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n

Hint: Write this by case splitting on n. In the case of S k, 
you can rewrite with a recursive call to myPlusCommutes k m, and rewrites can be nested. 
 -}
myPlusCommutative : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutative 0 m = sym (plusZeroRightNeutral m)
{-
Need to prove: S (plus k m) = plus m (S k)

prf1 : plus k m = plus m k
prf2 : S (plus m k) = plus m (S k)

Using prf1,
Rewrite S (plus m k) = plus m (S k)
as      S (plus k m) = plus m (S k) 

 -}
myPlusCommutative (S k) m = let prf1 = myPlusCommutative k m
                                prf2 = plusSuccRightSucc m k
                                in 
                                rewrite prf1 in prf2


-- Exercise 2

reverseProof_nil : Vect k a -> Vect (plus k 0) a
reverseProof_nil xs = rewrite plusZeroRightNeutral k in xs 

reverseProof_xs : Vect (S (plus k len)) a -> Vect (plus k (S len)) a
reverseProof_xs xs = rewrite sym (plusSuccRightSucc k len) in xs

myReverse2 : Vect n a -> Vect n a
myReverse2 xs = reverse' [] xs where
                reverse' : Vect k a -> Vect m a -> Vect (k + m) a
                reverse' acc [] = reverseProof_nil acc
                reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs)
