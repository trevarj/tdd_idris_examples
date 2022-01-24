import Data.List

data MySnocList : List a -> Type where
     Empty : MySnocList []
     Snoc : (x, xs : _) -> (rec : MySnocList xs) -> MySnocList (xs ++ [x])

snocListHelp : {input : _} -> (snoc : MySnocList input) -> (rest : List a) -> MySnocList (input ++ rest)
snocListHelp snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp snoc (x :: xs) = rewrite appendAssociative input [x] xs 
                              in snocListHelp (Snoc x input snoc) xs

mySnocList : (xs : List a) -> MySnocList xs
mySnocList [] = Empty
mySnocList xs = snocListHelp Empty xs

myReverseHelper : (input : List a) -> MySnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc x xs rec) = x :: myReverseHelper xs rec

-- myReverse : List a -> List a
-- myReverse input = myReverseHelper input (mySnocList input)

myReverse : List a -> List a 
myReverse input with (mySnocList input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (Snoc x xs rec) = x :: myReverse xs | rec

isSuffix : Eq a => List a -> List a -> Bool
isSuffix xs ys with (mySnocList xs, mySnocList ys)
  isSuffix _ _ | (Snoc x xs xsrec, Snoc y ys ysrec) = (x == y) && (isSuffix _ _ | (xsrec, ysrec))
  isSuffix _ _ | (Empty, s) = True
  isSuffix _ _ | (s, Empty) = False



