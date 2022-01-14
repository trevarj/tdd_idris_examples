
TupleVect : (n : Nat) -> Type -> Type
TupleVect 0 _ = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())