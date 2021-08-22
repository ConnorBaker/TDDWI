module DataStore

import Data.Vect

%default total

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command : Type where
  Quit   :                   Command
  Size   :                   Command
  Add    : (item : String) -> Command
  Search : (item : String) -> Command
  Get    : (idx  :    Nat) -> Command

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

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
getEntry idx store = 
  case natToFin idx (size store) of
    Nothing => Just ("Out of range\n", store)
    Just  m => Just (index m (items store) ++ "\n", store)

searchEntry : String -> DataStore -> Maybe (String, DataStore)
searchEntry item store =
  case fst (foldl go ("", 0) (items store)) of
    ""   => Just ("No results found\n", store)
    hits => Just (hits, store)
  where
    go : (String, Nat) -> String -> (String, Nat)
    go (hits, n) possibleHit =
      case Strings.isInfixOf item possibleHit of
        False => (hits, n+1)
        True  => (hits ++ show n ++ ": " ++ possibleHit ++ "\n", n+1)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse inp of
    Nothing            => Just ("Invalid command\n", store)
    Just (Quit       ) => Nothing
    Just (Size       ) => Just (show (size store) ++ "\n", store)
    Just (Add    item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (Search item) => searchEntry item store
    Just (Get     idx) => getEntry idx store

partial main : IO ()
main = replWith (MkData _ []) "Command: " processInput
