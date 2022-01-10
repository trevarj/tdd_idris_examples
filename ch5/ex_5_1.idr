{-

Using do notation, write a printLonger program that reads two strings and then displays the length of the longer string.

Write the same program using >>= instead of do notation. 

 -}

printLonger : IO ()
printLonger = do putStr "First string: "
                 str1 <- getLine
                 putStr "Second string: "
                 str2 <- getLine
                 let len1 = length str1
                 let len2 = length str2
                 let longer = if len1 > len2
                              then str1
                              else str2
                 putStrLn (longer ++ " is longer with length " ++ show (length longer))

printLonger2 : IO ()
printLonger2 = putStr "First string: " >>= \_ =>
               getLine >>= \str1 =>
               let len1 = length str1 in
               putStr "Second string: " >>= \_ =>
               getLine >>= \str2 =>
               let len2 = length str2 in
               let longer = if len1 > len2
                            then str1
                            else str2 in
               putStrLn (longer ++ " is longer with length " ++ show (length longer))