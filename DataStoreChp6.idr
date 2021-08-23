module Main

import Data.Vect

%default total

infixr 5 .+.

data Schema : Type where
  SString :                     Schema
  SInt    :                     Schema
  (.+.)   : Schema -> Schema -> Schema

SchemaType : Schema -> Type
SchemaType schema =
  case schema of
    (SString) => String
    (SInt   ) => Int
    (x .+. y) => (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size   : Nat
  items  : Vect size (SchemaType schema)

addToStore : (ds : DataStore) -> SchemaType (schema ds) -> DataStore
addToStore ds y = MkData _ _ (items ds ++ [y])

-- TODO!
display : (schema : Schema) -> SchemaType schema -> String

getEntry : Nat -> DataStore -> (String, DataStore)
getEntry idx ds@(MkData schema size items) =
  case natToFin idx size of
    Nothing => ("Out of range\n", ds)
    Just  m => ((display schema) (index m items) ++ "\n", ds)

searchEntry : String -> DataStore -> (String, DataStore)
searchEntry itemStr ds@(MkData schema size items) =
  if isNil hits
    then ("No results found\n", ds)
    else (unlines hits ++ "\n", ds)
where
  hits : List String
  hits = map (\ (idx, itemStr') => show (finToNat idx) ++ ": " ++ itemStr')
       . filter ((==) itemStr . snd) -- Keep only entries with matching values
       . toList -- Convert to a list to avoid a dependent pair
       . zip range -- Attach indices
       . map (display schema) -- Stringify the db
       $ items

{-
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
-}