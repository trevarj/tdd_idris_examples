
%default total

data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
     Seq : IO () -> Inf (RunIO b) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

(>>) : IO () -> Inf (RunIO b) -> RunIO b
(>>) = Seq

greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
              then do putStrLn "Bye!"
                      Quit ()
              else do putStrLn ("Hello " ++ name)
                      greet
