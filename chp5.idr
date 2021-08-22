import System
import Data.Vect

%default total

printLength : IO ()
printLength = getLine >>= (putStrLn . show . length)

printInput : IO ()
printInput = getLine >>= putStrLn

printLonger : IO ()
printLonger = do
  putStrLn "First string: "
  s1 <- getLine
  putStrLn "Second string: "
  s2 <- getLine
  let longerS = longerStr s1 s2
  putStrLn (longerS ++ " with length " ++ show (length longerS))
where
  longerStr : String -> String -> String
  longerStr s1 s2 =
    if (length s1) < (length s2)
      then s2
      else s1

parseNat : String -> Maybe Nat
parseNat str = 
  case all isDigit (unpack str) of
    False => Nothing
    True  => Just (cast str)

partial guess : Nat -> Nat -> IO ()
guess target numGuesses = do
  putStr $ "(" ++ show numGuesses ++ " guesses) Guess a number: "
  guessedStr <- getLine
  case parseNat guessedStr of
    Nothing      => putStrLn "Not a valid guess!" *> guess target (numGuesses + 1)
    Just guessed =>
      case compare guessed target of
        LT => putStrLn "Too low!"  *> guess target (numGuesses + 1)
        EQ => putStrLn "You did it!"
        GT => putStrLn "Too high!" *> guess target (numGuesses + 1)

partial readToBlank : IO (List String)
readToBlank = getLine >>= \ l => if l == "" then pure [] else liftA (l::) readToBlank

partial readAndSave : IO ()
readAndSave = do
  putStrLn "Reading input. Blank line to stop."
  ls <- readToBlank
  putStr "Enter a filename: "
  filename <- getLine
  Right () <- writeFile filename (unlines ls)
  | Left err => putStrLn (show err)
  pure ()

partial readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- openFile filename Read
  | Left err => pure (_ ** [])
  Right vect <- go file
  | Left err => pure (_ ** [])
  closeFile file
  pure vect
where
  partial go : File -> IO (Either FileError (n ** Vect n String))
  go file = do
    eof <- fEOF file
    case eof of
      True  => pure (Right (_ ** []))
      False => do
        Right l <- fGetLine file
        | Left err => pure (Left err)
        Right (_ ** ls) <- go file
        | Left err => pure (Left err)
        pure (Right (_ ** l :: ls))

partial main : IO ()
main = do
  t <- time
  guess (cast t) 0
