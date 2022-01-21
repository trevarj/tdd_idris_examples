import Data.Vect
import Data.Vect.Elem
import Decidable.Equality
import Data.String
import RemoveElem

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) ->
                   (missing : Vect letters Char) ->
                   WordState guesses_remaining letters

data Finished : Type where
     Lost : (game : WordState 0 (S letters)) -> Finished
     Won  : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

-- proofs
isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No isValidNil where
                     isValidNil : ValidInput [] -> Void
                     isValidNil (Letter c) impossible 
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No isValidTooLong where
                                   isValidTooLong : ValidInput (x :: y :: xs) -> Void
                                   isValidTooLong (Letter _ ) impossible

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

-- game logic
readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess: "
               x <- getLine
               case isValidString (toUpper x) of
                    Yes prf => pure (_ ** prf)
                    No contra => do putStrLn "Invalid guess"
                                    readGuess

processGuess : {letters : _} ->
               (letter : Char) -> 
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      Yes prf => Right (MkWordState word (removeElem letter missing))
                                                      No contra => Left (MkWordState word missing)

game : {guesses : _} -> {letters : _} -> WordState (S guesses) (S letters) -> IO Finished
game st = do (_ ** Letter letter) <- readGuess
             case processGuess letter st of
                  Left l => do putStrLn "Wrong!"
                               case guesses of
                                    0 => pure (Lost l)
                                    (S k) => game l
                  Right r => do putStrLn "Right!"
                                case letters of
                                     0 => pure (Won r)
                                     (S k) => game r

main : IO ()
main = do result <- game {guesses = 2} (MkWordState "Test" ['T', 'E', 'S'])
          case result of
               Lost (MkWordState word missing) => putStrLn ("You lost. The word was " ++ word)
               Won  (MkWordState word missing) => putStrLn ("You won! The word was " ++ word)