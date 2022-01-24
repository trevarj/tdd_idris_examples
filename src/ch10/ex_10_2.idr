import Data.List.Views
import Data.List.Views.Extra
import Data.Nat.Views

||| Returns maximum equal suffix of two input lists
-- idris2 cannot check inside pairs so totality check fails
-- equalSuffix : Eq a => List a -> List a -> List a
-- equalSuffix input1 input2 with (snocList input1, snocList input2)
--   equalSuffix (xs ++ [x]) (ys ++ [y]) | ((Snoc x xs xsrec), (Snoc y ys ysrec)) 
--     = if x == y then equalSuffix xs ys ++ [x] else []
--   equalSuffix _ _ | (Empty, _) = []
--   equalSuffix _ _ | (_, Empty) = []

||| Returns maximum equal suffix of two input lists
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] _ | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc x xs rec) with (snocList input2)
    equalSuffix (xs ++ [x]) [] | (Snoc x xs rec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc x xs rec) | (Snoc y ys z) 
        = if x == y then equalSuffix xs ys ++ [x] else []

-- with recursion, but bug
-- equalSuffix : Eq a => List a -> List a -> List a
-- equalSuffix input1 input2 with (snocList input1)
--   equalSuffix [] _ | Empty = []
--   equalSuffix (xs ++ [x]) input2 | (Snoc x xs recxs) with (snocList input2)
--     equalSuffix (xs ++ [x]) [] | (Snoc x xs recxs) | Empty = []
--     equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc x xs recxs) | (Snoc y ys recys)
--         = if x == y then (equalSuffix xs ys | recxs | recys) ++ [x] else []

||| Turns Nat into binary string (slow)
toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary 0 | HalfRecZ = ""
  toBinary (k + k) | (HalfRecEven k rec) = (toBinary k | rec) ++ "0"
  toBinary (S (k + k)) | (HalfRecOdd k rec) = (toBinary k | rec) ++ "1"

||| Returns True if a List is traversed the same forwards and backwards
palindrome : Eq a => List a -> Bool
palindrome input with (vList input)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) = 
    if x == y then (palindrome xs | rec) else False

