module Main

import Data.Vect

%default total

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command : Type where
  Quit   :                    Command
  Size   :                    Command
  Add    : (item : String) -> Command
  Search : (item : String) -> Command
  Get    : (idx  :    Nat) -> Command

addToStore : DataStore -> String -> DataStore
addToStore (MkData _ items) y = MkData _ (items ++ [y])

getEntry : Nat -> DataStore -> (String, DataStore)
getEntry idx ds@(MkData size items) =
  case natToFin idx size of
    Nothing => ("Out of range\n", ds)
    Just  m => (index m items ++ "\n", ds)

searchEntry : String -> DataStore -> (String, DataStore)
searchEntry item ds@(MkData size items) =
  if isNil hits
    then ("No results found\n", ds)
    else (unlines hits ++ "\n", ds)
where
  indexHits : List (Fin size)
  indexHits = elemIndicesBy Strings.isInfixOf item items

  hits : List String
  hits = map (\idx => show (finToNat idx) ++ ": " ++ index idx items) indexHits

parseCommand : String -> String -> Maybe Command
parseCommand "quit"   ""   = Just (Quit       )
parseCommand "size"   ""   = Just (Size       )
parseCommand "add"    item = Just (Add    item)
parseCommand "search" item = Just (Search item)
parseCommand "get"    idx  =
  case all isDigit (unpack idx) of
    False => Nothing
    True  => Just (Get (cast idx))
parseCommand _ _ = Nothing

parse : String -> Maybe Command
parse = (\ (cmd, args) => parseCommand cmd (ltrim args)) . span (/= ' ')

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds@(MkData size _) inp =
  case parse inp of
    Nothing            => Just ("Invalid command\n", ds)
    Just (Quit       ) => Nothing
    Just (Size       ) => Just (show size ++ "\n", ds)
    Just (Add    item) => Just ("ID " ++ show size ++ "\n", addToStore ds item)
    Just (Search item) => Just (searchEntry item ds)
    Just (Get     idx) => Just (getEntry idx ds)

partial main : IO ()
main = replWith (MkData _ []) "Command: " processInput
