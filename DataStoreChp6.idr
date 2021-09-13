module Main

import Data.Vect

infixr 5 .+.

data Schema : Type where
  SString :                     Schema
  SInt    :                     Schema
  (.+.)   : Schema -> Schema -> Schema

SchemaType : Schema -> Type
SchemaType (SString) = String
SchemaType (SInt   ) = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size   : Nat
  items  : Vect size (SchemaType schema)

addToStore : (ds : DataStore) -> SchemaType (schema ds) -> DataStore
addToStore ds x = MkData _ _ (items ds ++ [x])

setSchema : (ds : DataStore) -> Schema -> Maybe DataStore
setSchema ds schema =
  case size ds of
    (Z  ) => Just (MkData schema _ [])
    (S k) => Nothing

data Command : Schema -> Type where
  Quit      :                      Command schema
  SetSchema : Schema            -> Command schema
  Add       : SchemaType schema -> Command schema
  Get       : Nat               -> Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) =
      case span (/= '"') xs of
        (quoted, '"' :: input) => Just (pack quoted, ltrim (pack input))
        _                      => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = 
  case span isDigit input of
    ("",      _) => Nothing
    (num, input) => Just (cast num, ltrim input)
parsePrefix (schemaL .+. schemaR) input = do
  (valL,  input') <- parsePrefix schemaL input
  (valR, input'') <- parsePrefix schemaR input'
  Just ((valL, valR), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema x = 
  case parsePrefix schema x of
    Just (res, "") => Just res
    _              => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) =
  case xs of
    [] => Just SString
    _  =>
      case parseSchema xs of
        Nothing       => Nothing
        Just xsSchema => Just (SString .+. xsSchema)
parseSchema ("Int" :: xs) =
  case xs of
    [] => Just SInt
    _  =>
      case parseSchema xs of
        Nothing       => Nothing
        Just xsSchema => Just (SInt .+. xsSchema)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (command : String) -> (input : String) -> Maybe (Command schema)
parseCommand schema "add" input =
  case parseBySchema schema input of
    Nothing    => Nothing
    Just entry => Just (Add entry)
parseCommand schema "get" input = 
  case all isDigit (unpack input) of
    False => Nothing
    True  => Just (Get (cast input))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" input =
  case parseSchema (words input) of
    Nothing          => Nothing
    Just validSchema => Just (SetSchema validSchema)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema =   SString} (item        ) = show item
display {schema =      SInt} (item        ) = show item
display {schema = (y .+. z)} (itemL, itemR) = display itemL ++ ", " ++ display itemR

getEntry : (idx : Nat) -> (ds : DataStore) -> (String, DataStore)
getEntry idx ds@(MkData schema size items) =
  case natToFin idx size of
    Nothing => ("Index is out of range\n", ds)
    Just  m => ((display (index m items)) ++ "\n", ds)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Nothing                  => Just ("Invalid command\n", store)
    Just (Add          item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (SetSchema schema') =>
      case setSchema store schema' of
        Nothing     => Just ("Can't update schema when there are entries in the store\n", store)
        Just store' => Just ("OK\n", store')
    Just (Get           pos) => Just (getEntry pos store)
    Just (Quit             ) => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput

{-
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
-}
