module Main

import Data.Vect
import System.REPL
import Data.String
import Data.List

infixr 5 .+.

public export
data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

export
record DataStore where
     constructor MkData 
     schema : Schema
     size : Nat
     items: Vect size (SchemaType schema)

-- Store commands

export
empty : Schema -> DataStore
empty schema = MkData schema 0 []

export
addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem = MkData schema _ (addToData items)
    where
        addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
        addToData [] = [newitem]
        addToData (item :: items) = item :: addToData items

display : {schema : _} -> SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getItem : Integer -> (store : DataStore) -> Maybe (String, DataStore)
getItem i store = case integerToFin i (size store) of
                    Nothing => Just ("Entry not found\n", store)
                    Just i => Just (display (index i (items store)) ++ "\n", store)

getAllItems : (store : DataStore) -> Maybe (String, DataStore)
getAllItems store = let all = printAll (items store) in if all /= "" 
                                                           then Just (all, store)
                                                           else Just ("Store empty.\n", store) where 
                              printAll : Vect n (SchemaType (schema store)) -> String
                              printAll [] = ""
                              printAll (x :: xs) = display x ++ "\n" ++ printAll xs

search : String -> DataStore -> Maybe (String, DataStore)
search substr store = Just (searchItems (items store), store) where 
                            searchItems : Vect n (SchemaType (schema store)) -> String
                            searchItems [] = "No entries with " ++ substr ++ " found\n"
                            searchItems (x :: xs) = case isInfixOf substr (display x) of
                                                          False => searchItems xs
                                                          True => show (length xs) ++ ": " ++ display x ++ "\n"

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing


-- Store View 
public export
data StoreView : (store : DataStore) -> Type where
     SNil : (schema : _) -> StoreView (Main.empty schema)
     SAdd : (entry, store : _) -> (rec : StoreView store) ->
            StoreView (addToStore store entry)

storeViewHelp : {size : _} -> (store : _) -> (items : Vect size (SchemaType (schema store))) -> StoreView (MkData (schema store) size items)
storeViewHelp store [] = SNil (schema store)
storeViewHelp store (val :: xs) = SAdd val store (storeViewHelp store xs)

export
storeView : (store : DataStore) -> StoreView store
storeView store@(MkData schema size items) = storeViewHelp store items

-- -- REPL UI

data Command : Schema -> Type where 
     SetSchema : (newSchema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Maybe Integer -> Command schema
     Search : String -> Command schema
     Size : Command schema
     Quit : Command schema

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Nothing => Nothing
                                  Just (res, "") => Just res
                                  Just _ => Nothing where
                             parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
                             parsePrefix SString input = getQuoted (unpack input) where 
                                   getQuoted : List Char -> Maybe (String, String)
                                   getQuoted ('"' :: xs) = case span (/= '"') xs of
                                                                (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                                                _ => Nothing
                                   getQuoted _ = Nothing
                             parsePrefix SInt input = case span isDigit input of
                                                           ("", rest) => Nothing
                                                           (num, rest) => Just (cast num, ltrim rest)
                             parsePrefix SChar input = map (\(ch, rest) => (ch, ltrim rest)) (strUncons input)
                             parsePrefix (x .+. y) input = do (l_val, input') <- parsePrefix x input
                                                              (r_val, input'') <-  parsePrefix y input'
                                                              Just ((l_val, r_val), input'')

parseSchema : List String -> Maybe Schema
parseSchema (x :: xs) = do ty <- case x of
                                      "String" => Just SString
                                      "Int" => Just SInt
                                      "Char" => Just SChar
                                      _ => Nothing
                           case xs of
                                [] => Just ty
                                _ => map (\s => ty .+. s) (parseSchema xs)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema cmd rest = case cmd of
                              "schema" => map SetSchema (parseSchema (words rest))
                              "add" => map Add (parseBySchema schema rest)
                              "get" => map Get (Just (parseInteger rest))
                              "search" => Just (Search rest)
                              "size" => Just Size
                              "quit" => Just Quit
                              _ => Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

processCommand : (store : DataStore) -> Command (schema store) -> Maybe (String, DataStore)
processCommand store (SetSchema newSchema) = case setSchema store newSchema of
                                                  Nothing => Just ("Cannot update schema for non-empty store\n", store)
                                                  Just newStore => Just ("Schema updated\n", newStore)

processCommand store (Add item) = Just ("Added ID " ++ show (size store) ++ " to store\n", addToStore store item)
processCommand store (Get (Just i)) = getItem i store
processCommand store (Get Nothing) = getAllItems store
processCommand store Size = Just (show (size store) ++ " entries in store\n", store)
processCommand store (Search substr) = search substr store
processCommand _ Quit = Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                Just cmd => processCommand store cmd


main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
