import Data.Vect

%default total

Mat : Nat -> Nat -> Type -> Type
Mat n m a = Vect n (Vect m a)

allLengths : Vect len String -> Vect len Nat
allLengths = map length

insert : Ord a => (x : a) -> (xsSorted : Vect len a) -> Vect (S len) a
insert x [          ] = [x]
insert x xs@(y :: ys) =
  if x < y
    then x :: xs
    else y :: insert x ys

insSort : Ord a => Vect n a -> Vect n a
insSort [       ] = []
insSort (x :: xs) = insert x (insSort xs)

createEmpties : Vect n (Vect 0 a)
createEmpties = replicate _ []

transposeHelper : (x : Vect n a) -> (xsTrans : Vect n (Vect k a)) -> Vect n (Vect (S k) a)
transposeHelper [       ] [       ] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [       ] = createEmpties
transposeMat (x :: xs) = transposeHelper x (transposeMat xs)

transposeMat' : Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat' [       ] = createEmpties
transposeMat' (x :: xs) = zipWith (::) x (transposeMat xs)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix = zipWith (zipWith (+))

dotProd : Num a => Vect n a -> Vect n a -> a
dotProd xs ys = sum (zipWith (*) xs ys)

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [ ] _   = []
multMatrix xss yss = go xss (transpose yss)
  where
    go : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
    go [         ] tss = []
    go (xs :: xss) tss = map (dotProd xs) tss :: go xss tss
