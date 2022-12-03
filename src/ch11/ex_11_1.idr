import Data.Stream

import InfList
import Streams

-- 1.
-- take 10 (every_other [1..])
-- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20] : List Integer
every_other : Stream a -> Stream a
every_other (x :: y :: xs) = y :: every_other xs

-- 2.
-- getPrefix 10 (map (*2) (countFrom 1))
-- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20] : List Integer
Functor InfList where
    map f (val :: vals) = f val :: map f vals


-- 3.
-- coinFlips 6 (randoms 12345)
-- [Tails, Heads, Tails, Tails, Heads, Tails] : List Face
data Face = Heads 
          | Tails

getFace : Int -> Face
getFace x = case mod x 2 == 0 of
                 False => Tails
                 True => Heads


coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips 0 xs = []
coinFlips (S k) (x :: xs) = getFace x :: coinFlips k xs

-- 4.
-- take 3 (square_root_approx 10 10)
-- [10.0, 5.5, 3.659090909090909] : List Double
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next = (approx + (number / approx)) / 2 
                                     in approx :: square_root_approx number next

{-
5. 

*ex_11_1> square_root 6
2.449489742783178 : Double

*ex_11_1> square_root 2500
50.0 : Double

*ex_11_1> square_root 2501
50.009999000199954 : Double

 -}
square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound 0 _ _ (x :: xs) = x
square_root_bound (S k) number bound (approx :: approxs) 
                = if approx * approx - number < bound
                    then approx
                    else square_root_bound k number bound approxs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                 (square_root_approx number number)
