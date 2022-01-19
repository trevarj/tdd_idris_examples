import Decidable.Equality

data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

isNotNill : Last [] value -> Void
isNotNill LastOne impossible
isNotNill (LastCons prf) impossible

isNotLastOne : (value = x -> Void) -> Last [x] value -> Void
isNotLastOne notLastOne LastOne = notLastOne Refl
isNotLastOne _ (LastCons prf) = isNotNill prf

notInTail : (xs = [] -> Void) -> 
            (Last xs value -> Void) -> 
            Last (x :: xs) value -> Void
notInTail hasMore noMore LastOne = hasMore Refl
notInTail hasMore noMore (LastCons later) = noMore later


isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No isNotNill
isLast (x :: xs) value 
    = case decEq xs [] of
           Yes Refl => case decEq value x of
                            Yes Refl => Yes LastOne
                            No notLastOne => No (isNotLastOne notLastOne)
           No hasMore => case isLast xs value of
                              Yes prf => Yes (LastCons prf)
                              No noMore => No (notInTail hasMore noMore)


