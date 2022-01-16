import Data.Vect

append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + len)) elem -> Vect (m + (S len)) elem
append_xs xs = rewrite sym (plusSuccRightSucc m len) in xs

-- by "carelessly" changing `n + m` to `m + n` we need use a few rewrites
-- to prove to idris that the definition is valid
append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)