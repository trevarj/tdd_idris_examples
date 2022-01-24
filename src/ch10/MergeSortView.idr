import Data.List
import Data.List.Views

{-
Note: 
The | says that, in the recursive calls, 
you want to bypass constructing the view, 
because you already have appropriate views for lefts and rights. 
-}

mergeSort : Ord a => List a -> List a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | (SplitRecOne x) = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lefts rights lrec rrec) 
            = merge (mergeSort lefts | lrec) (mergeSort rights | rrec)

