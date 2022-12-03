import Control.Monad.State
import TreeLabelType

{-
1.
runState 89 (increase 5)
((), 94) : ((), Nat)
-}
update : (stateType -> stateType) -> State stateType ()
update f = do current <- get
              put (f current)

increase : Nat -> State Nat ()
increase x = update (+x)

{-
2.
execState 0 (countEmpty testTree)
7 : Nat
-}
countEmpty : Tree a -> State Nat ()
countEmpty Empty = update (+1)
countEmpty (Node left _ right) = do countEmpty left
                                    countEmpty right

{-
3.
execState (0, 0) (countEmptyNode testTree)
(7, 6) : (Nat, Nat)
 -}
countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = update (\x => (fst x + 1, snd x))
countEmptyNode (Node left _ right) = do update (\x => (fst x, snd x + 1))
                                        countEmptyNode left
                                        countEmptyNode right