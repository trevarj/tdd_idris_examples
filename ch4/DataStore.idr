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
                                     Nothing => Just ("Entry not found", store)
                                     Just i => Just (index i items ++ "\n", store)

-- REPL UI

data Command = Add String
             | Get Integer
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" i   = map (\i => Get i) (parseInteger i) 
parseCommand "quit" _  = Just Quit
parseCommand _ _       = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)



processCommand : DataStore -> Command -> Maybe (String, DataStore)
processCommand store (Add str) = Just ("Added ID " ++ show (size store) ++ " to store\n", addToStore store str)
processCommand store (Get i) = getItem i store
processCommand _ Quit = Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                Just cmd => processCommand store cmd


main : IO ()
main = replWith (MkData _ []) "Command: " processInput