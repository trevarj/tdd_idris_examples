import Data.Vect

-- Chapter 3.2 Exercises

my_length : List a -> Nat
my_length [] = Z
my_length (x :: xs) = S (my_length xs)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse [x] = [x]
my_reverse (x :: xs) = my_reverse xs ++ [x]

my_map : (a -> b) -> List a -> List b
my_map _ [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_map_vect : (a -> b) -> Vect n a -> Vect n b
my_map_vect _ [] = []
my_map_vect f (x :: xs) = f x :: my_map_vect f xs