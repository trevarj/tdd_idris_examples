import Data.Vect
import Decidable.Equality

data Elem : a -> Vect k a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notThere : Elem value xs -> Void) ->
            (notHere : (value = x) -> Void) ->
            Elem value (x :: xs) -> Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later


isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) 
    = case decEq value x of
           Yes Refl => Yes Here
           No notHere => case isElem value xs of
                              Yes prf => Yes (There prf)
                              No notThere => No (notInTail notThere notHere)

-- similar example for checking if an element is in a Vect using Booleans
elem : Eq ty => (value : ty) -> (xs : Vect n ty) -> Bool
elem value [] = False
elem value (x :: xs) = case value == x of
                            False => Main.elem value xs
                            True => True


