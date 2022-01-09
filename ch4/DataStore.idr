module Main

import Data.Vect
import System.REPL
import Data.String

data DataStore : Type where
     MkData    : (size : Nat) ->
                 (items: Vect size String) ->
                 DataStore

-- Store commands 
size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
    where
        addToData : Vect old String -> Vect (S old) String
        addToData [] = [newitem]
        addToData (item :: items) = item :: addToData items

getItem : Integer -> DataStore -> Maybe (String, DataStore)
getItem i store@(MkData size items) = case integerToFin i size of
                                     Nothing => Just ("Entry not found\n", store)
                                     Just i => Just (index i items ++ "\n", store)

search : String -> DataStore -> Maybe (String, DataStore)
search substr store@(MkData size items) = Just (searchItems items, store) where 
                                          searchItems : Vect n String -> String
                                          searchItems [] = "No entries with " ++ substr ++ " found\n"
                                          searchItems (x :: xs) = case isInfixOf substr x of
                                                                        False => searchItems xs
                                                                        True => show (length xs) ++ ": " ++ x ++ "\n"

-- REPL UI

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand cmd input = case cmd of
                              "add" => Just (Add input)
                              "get" => map (\i => Get i) (parseInteger input)
                              "search" => Just (Search input)
                              "size" => Just Size
                              "quit" => Just Quit
                              _ => Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

processCommand : DataStore -> Command -> Maybe (String, DataStore)
processCommand store (Add str) = Just ("Added ID " ++ show (size store) ++ " to store\n", addToStore store str)
processCommand store (Get i) = getItem i store
processCommand store Size = Just (show (size store) ++ " entries in store\n", store)
processCommand store (Search substr) = search substr store
processCommand _ Quit = Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                Just cmd => processCommand store cmd


main : IO ()
main = replWith (MkData _ []) "Command: " processInput