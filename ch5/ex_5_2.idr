import Data.String
import System

readNumber : IO (Maybe Nat)
readNumber =
    do numstr <- getLine
       pure (parsePositive numstr)

-- worst random number generator
rand : (min : Integer) -> (max : Integer) -> IO (Nat)
rand min max = do secs <- time
                  let n = (mod secs (max - min + 1)) + 1
                  pure (integerToNat n)

-- Guess the number game
guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses =
               do putStrLn ("[Guess count: " ++ show guesses ++ "]")
                  putStrLn "Enter guess: " 
                  Just num <- readNumber 
                            | Nothing => do putStrLn "Invalid input"
                                            guess target guesses
                  case compare num target of
                       LT => do putStrLn "Too low."
                                guess target (guesses + 1)
                       EQ => putStrLn "Correct!"
                       GT => do putStrLn "Too high."
                                guess target (guesses + 1)

main : IO ()
main = do
        target <- rand 1 100
        guess target 0