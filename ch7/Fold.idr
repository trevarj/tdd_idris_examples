
totalLen : List String -> Nat
totalLen [] = 0
totalLen xs = foldr (\str, len => length str + len) 0 xs

