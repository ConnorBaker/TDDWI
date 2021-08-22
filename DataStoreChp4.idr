module DataStoreChp4

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
addToStore (MkData size items) y = MkData (size + 1) (items ++ [y])

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

getEntry : Nat -> DataStore -> Maybe (String, DataStore)
getEntry idx ds@(MkData size items) =
  case natToFin idx size of
    Nothing => Just ("Out of range\n", ds)
    Just  m => Just (index m items ++ "\n", ds)

searchEntry : String -> DataStore -> Maybe (String, DataStore)
searchEntry item ds@(MkData _ items) =
  case fst (foldl go ("", 0) items) of
    ""   => Just ("No results found\n", ds)
    hits => Just (hits, ds)
  where
    go : (String, Nat) -> String -> (String, Nat)
    go (hits, n) possibleHit =
      case Strings.isInfixOf item possibleHit of
        False => (hits, n+1)
        True  => (hits ++ show n ++ ": " ++ possibleHit ++ "\n", n+1)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds@(MkData size items) inp =
  case parse inp of
    Nothing            => Just ("Invalid command\n", ds)
    Just (Quit       ) => Nothing
    Just (Size       ) => Just (show size ++ "\n", ds)
    Just (Add    item) => Just ("ID " ++ show size ++ "\n", addToStore ds item)
    Just (Search item) => searchEntry item ds
    Just (Get     idx) => getEntry idx ds

partial main : IO ()
main = replWith (MkData _ []) "Command: " processInput
