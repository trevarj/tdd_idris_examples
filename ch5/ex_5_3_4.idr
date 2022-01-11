import Data.Vect
import Data.String
import System.File.Handle
import System.File.Mode
import System.File.ReadWrite

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if (x == "")
                then pure (_ ** [])
                else do (_ ** xs) <- readVect
                        pure (_ ** x :: xs)

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- readVect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vec2 => printLn (zip vec1 vec2)

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if x == ""
                    then pure ([])
                    else do xs <- readToBlank
                            pure (x :: xs)

||| Read until blank line then
||| Read file name and save entered data to file
readAndSave : IO ()
readAndSave = do putStrLn "Type some lines (blank line to end):"
                 list <- readToBlank
                 putStrLn "Enter file name:"
                 fileName <- getLine
                 Left err <- writeFile fileName (unlines list)
                           | Right () => pure () 
                 printLn err

msg : String -> Maybe FileError -> IO (n ** Vect n String)
msg m err = let m_ = case err of
                          Nothing => m
                          Just err => m ++ ": " ++ (show err)
                     in do putStrLn m_
                           pure (_ ** [])

||| Reads file contents into Vect
||| ex.
|||   :exec readVectFile "test.txt" >>= printLn
readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right file <- openFile filename Read
                                       | Left err => msg "Could not open file" (Just err)
                           nextLine file where
                           nextLine : File -> IO (n ** Vect n String)
                           nextLine f = do eof <- fEOF f
                                           if eof then msg "Done reading file" Nothing
                                            else do Right line <- fGetLine f
                                                                | Left err => msg "Could not read line" (Just err)
                                                    (_ ** xs) <- nextLine f
                                                    pure (_ ** line :: xs)

handleErr : FileError -> IO String
handleErr err = pure (show err)

readFileToVect : File -> IO (Either String (n ** Vect n String))
readFileToVect f = do eof <- fEOF f
                      if eof then pure (Right (_ ** []))
                        else do Right line <- fGetLine f
                                                 | Left err => pure (Left (show err))
                                Right (_ ** xs) <- readFileToVect f
                                                 | Left err => pure (Left (show err))
                                pure (Right (_ ** line :: xs))

||| Reads file contents into Vect using withFile
||| Returns either an error message or the Vect of file contents
readVectFile2 : (filename : String) -> IO (Either String (n ** Vect n String))
readVectFile2 filename = do withFile filename Read handleErr readFileToVect