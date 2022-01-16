import Data.List -- for testing using sort

data Matter = Solid | Liquid | Gas

Eq Matter where
    (==) Solid Solid = True
    (==) Liquid Liquid = True
    (==) Gas Gas = True
    (==) _ _ = False

occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case item == x of
                                  False => occurrences item xs
                                  True => 1 + occurrences item xs

-- 7.1 exercises

data Shape = Triangle Double Double 
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle b h) = b * h * 0.5
area (Rectangle w h) = w * h
area (Circle r) = pi * pow r 2

Eq Shape where
    (==) (Triangle b h) (Triangle b' h') = b == b' && h == h'
    (==) (Rectangle w h) (Rectangle w' h') = w == w' && h == h'
    (==) (Circle r) (Circle r') = r == r'
    (==) _ _ = False

Ord Shape where
    compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]