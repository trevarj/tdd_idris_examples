
%default total

data InfIO : Type where
     Do : IO a 
          -> (a -> Inf InfIO)
          -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

loopPrint : String -> InfIO
-- displays and continues!
loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)

partial
run : InfIO -> IO ()
run (Do action cont) = do res <- action
                          run (cont res)

