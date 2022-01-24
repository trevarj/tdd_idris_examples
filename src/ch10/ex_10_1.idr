import Data.Nat

data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> {rest : _ } -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN _ [] = Fewer
takeN 0 _ = Exact [] -- there's some left, and n == Z so it must mean we have an exact amount
takeN (S k) (x :: xs) = case takeN k xs of
                             Fewer => Fewer
                             Exact n_xs => Exact (x :: n_xs)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

halves : List a -> (List a, List a)
halves xs with (takeN (div (length xs) 2) xs)
  halves xs | Fewer = ([], xs)
  halves (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)

