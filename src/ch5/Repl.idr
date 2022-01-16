||| Provide a prompt and apply function to user input
||| ex. myRepl "Enter a string: " reverse 
repl : (prompt : String) -> (String -> String) -> IO ()
repl prompt f = do putStrLn prompt
                   input <- getLine
                   putStrLn (f input)
                   repl prompt f

-- dummy helper for testing
processInput : List String -> String -> Maybe (String, List String)
processInput xs input = Just ("Added item, count = " ++ show (length xs + 1), input :: xs )

||| Given a state and prompt,  apply a function to the state and user input
||| ex. replWith [] "Enter a string: " processInput
replWith : (state: a) -> (prompt : String) -> (a -> String -> Maybe (String, a)) -> IO ()
replWith state prompt f = do putStrLn prompt
                             input <- getLine
                             let Just (output, newState) = f state input 
                               | Nothing => replWith state prompt f
                             putStrLn output
                             replWith newState prompt f