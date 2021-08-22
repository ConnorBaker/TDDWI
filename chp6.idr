import Data.Vect

%default total

data Format : Type where
  FEnd    :                     Format
  FInt    :           Format -> Format
  FDouble :           Format -> Format
  FChar   :           Format -> Format
  FString :           Format -> Format
  FLit    : String -> Format -> Format

PrintfType : Format -> Type
PrintfType (FEnd       ) = String
PrintfType (FInt      f) = Int    -> PrintfType f
PrintfType (FDouble   f) = Double -> PrintfType f
PrintfType (FChar     f) = Char   -> PrintfType f
PrintfType (FString   f) = String -> PrintfType f
PrintfType (FLit    s f) =           PrintfType f

printfFmt : (f : Format) -> (acc : String) -> PrintfType f
printfFmt (FEnd       ) acc =       acc
printfFmt (FInt      f) acc = \i => printfFmt f (acc ++ show i)
printfFmt (FDouble   f) acc = \d => printfFmt f (acc ++ show d)
printfFmt (FChar     f) acc = \c => printfFmt f (acc ++ singleton c)
printfFmt (FString   f) acc = \s => printfFmt f (acc ++ s)
printfFmt (FLit    s f) acc =       printfFmt f (acc ++ s)

toFormat : List Char -> Format
toFormat [                   ] = FEnd
toFormat ('%' :: 'd' :: chars) = FInt     (toFormat chars)
toFormat ('%' :: 'f' :: chars) = FDouble  (toFormat chars)
toFormat ('%' :: 'c' :: chars) = FChar    (toFormat chars)
toFormat ('%' :: 's' :: chars) = FString  (toFormat chars)
toFormat ('%' ::        chars) = FLit "%" (toFormat chars)
toFormat (c   ::        chars) =
  case toFormat chars of
    FLit lit chars' => FLit (strCons c lit) chars'
    f               => FLit (strCons c  "") f

printf : (f : String) -> PrintfType (toFormat (unpack f))
printf f = printfFmt _ ""

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : Nat -> Type -> Type
TupleVect (Z  ) t = (                )
TupleVect (S k) t = (t, TupleVect k t)

-- You could implement a vector as nested pairs, with the nesting calculated
-- from the length, as in this example:
test : TupleVect 4 Nat
test = (1,2,3,4,())
