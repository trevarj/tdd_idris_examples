{-
Write a function, maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a, 
that returns the larger of the two inputs, or Nothing if both inputs 
are Nothing. 

For example: 
> maxMaybe (Just 4) (Just 5)
Just 5 : Maybe Integer

> maxMaybe (Just 4) Nothing
Just 4 : Maybe Integer
 -}

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x y = if x > y then x else y -- Maybe is defined as Ord in the Prelude ;)