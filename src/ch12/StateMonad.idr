import Data.List

data State : (stateType : Type) -> Type -> Type where
     Get : State stateType stateType
     Put : stateType -> State stateType ()

     Pure : ty -> State stateType ty
     Bind : State stateType a -> (a -> State stateType b) -> State stateType b

get : State stateType stateType
get = Get

put : stateType -> State stateType ()
put = Put

mutual
  Functor (State stateType) where 
      map f x= do val <- x 
                  pure (f val)
  
  Applicative (State stateType) where 
      pure = Pure
      (<*>) f a = do f' <- f
                     a' <- a
                     pure (f' a')
  
  Monad (State stateType) where
      (>>=) = Bind

runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put newState) st = ((), newState)
runState (Pure x) st = (x, st)
runState (Bind cmd prog) st = let (val, nextState) = runState cmd st in 
                                                     runState (prog val) nextState

addIfPositive : Integer -> State Integer Bool
addIfPositive val = do when (val > 0) $
                          do current <- get
                             put (current + val)
                       pure (val > 0)

addPositives : List Integer -> State Integer Nat
addPositives vals = do added <- traverse addIfPositive vals
                       pure (length (filter id added))




