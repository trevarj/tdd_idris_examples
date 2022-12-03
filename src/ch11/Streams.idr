import Data.Stream
import Data.Bits

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals

label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in 
                  (seed' `shiftR` 2) :: randoms seed'