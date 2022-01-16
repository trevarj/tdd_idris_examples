
data Format = Number Format
            | NumDouble Format
            | Str Format
            | Ch Format
            | Lit String Format
            | End
%name Format fmt

||| Takes a Format and converts it to a Type that 
||| includes the variable length arguments to the format string
||| ex. 
|||    PrintfType (toFormat (unpack "this is a literal of %d words"))
||| will return Int -> String : Type
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (NumDouble fmt) =(d : Double) -> PrintfType fmt
PrintfType (Str fmt) = (s : String) -> PrintfType fmt
PrintfType (Ch fmt) = (c : Char) -> PrintfType fmt
PrintfType (Lit s fmt) = PrintfType fmt
PrintfType End = String

||| Builds String from a Format
||| Basically recursively parses the given Format into final PrintfType to return from printf
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt)  acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (NumDouble fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Ch fmt) acc = \c => printfFmt fmt (acc ++ cast c)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

||| Converts format string (with %d or %s) to Format
||| ex. toFormat (unpack "this is a literal of %d words")
|||  => Lit "this is a literal of " (Number (Lit " words" End)) : Format
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 'f' :: chars) = NumDouble (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Ch (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

||| PrintfType will generate a Type based on the input string `fmt`
||| which consists of all the arguments that need to be present
||| to interpolate the string.
||| > :t printf "the %s is %d years old" : String -> Int -> String
||| > printf "the %s is %d years old" "man" 55 : String
||| => "the man is 55 years old" : String
printf : (fmtStr : String) -> PrintfType (toFormat (unpack fmtStr))
printf _ = printfFmt _ "" -- full Format type inferred from above definition
