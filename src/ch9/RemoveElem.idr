import Data.Vect
import Data.Vect.Elem

-- removeElem : {n : _ } -> (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
-- removeElem value (value :: xs) Here = xs
-- removeElem {n = Z} value (y :: []) (There later) = absurd later
-- removeElem {n = (S k)} value (y :: xs) (There later) = y :: removeElem value xs later

-- removeElem_auto : {n : _} -> (value : a) -> 
--                   (xs : Vect (S n) a) -> 
--                   (prf : Elem value xs) =>
--                   Vect n a
-- removeElem_auto value xs = removeElem value xs prf

removeElem : {n : _ } -> 
             (value : a) -> 
             (xs : Vect (S n) a) -> 
             (prf : Elem value xs) => 
             Vect n a
removeElem value (value :: xs) {prf = Here} = xs
removeElem {n = Z} value (y :: []) {prf = (There later)} = absurd later
removeElem {n = (S k)} value (y :: xs) {prf = (There later)} = y :: removeElem value xs


