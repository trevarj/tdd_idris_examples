{-
A technique for adding more patterns to match on for data types
-}

data ListLast : List a -> Type where
     Empty : ListLast []
     NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

-- a view for ListLast
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty ys y => NonEmpty (x :: ys) y

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

describeListEndVerbose : List Int -> String
describeListEndVerbose [] = "Empty"
describeListEndVerbose (x :: xs) = describeHelper xs (listLast xs)

describeListEnd : List Int -> String
describeListEnd input with (listLast input)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (xs ++ [x]) | (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

-- non-total!!!
myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs


