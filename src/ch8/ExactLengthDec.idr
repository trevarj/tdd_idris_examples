import Data.Vect
import Decidable.Equality

myExactLength : {m : _} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
myExactLength len input = case decEq m len of
                                 Yes Refl => Just input
                                 No contra => Nothing