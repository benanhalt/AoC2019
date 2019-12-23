
import Data.List
import Data.Maybe
import Control.Applicative


main :: IO ()
main = do
  putStrLn "Part 1:"
  print $ findCard 2019 $ applySeq (parseAll input) $ factory 10007
  putStrLn "Part 2:"
  let doIt = applySeq $ parseAll input
  let result1 = doIt $ factory 119315717514047
  let traceCard i = i `seq` cardAt i $ result1
  let trace = iterate' traceCard 2020
  print $ trace !! 101741582076661

data Op
  = NewStack
  | Cut Int
  | Increment Int
  deriving Show

type Deck = (Int, Int -> Int)

factory :: Int -> Deck
factory n = (n, id)

toList :: Deck -> [Int]
toList (n, f) = f <$> [0..n-1]

findCard :: Int -> Deck -> Maybe Int
findCard j d = elemIndex j $ toList d

cardAt :: Int -> Deck -> Int
cardAt i (n, f) = f i

applyOp :: Op -> Deck -> Deck
applyOp NewStack (n, f) = (n, f. (\i -> n - 1 - i))
applyOp (Cut m) (n, f) = (n, f . (\i -> (i + m) `mod` n))
applyOp (Increment m) (n, f) = (n, f . (\i -> (i + n*(l i)) `div` m))
  where l i = inverse n m * (-i) `mod` m

applySeq :: [Op] -> Deck -> Deck
applySeq ops d = foldl' (flip applyOp) d ops

parseAll :: [String] -> [Op]
parseAll = catMaybes . (parse <$>)

parse :: String -> Maybe Op
parse s = parseInc s <|> parseNew s <|> parseCut s

parseInc :: String -> Maybe Op
parseInc = (Increment <$>) . (read <$>) . stripPrefix "deal with increment "

parseNew :: String -> Maybe Op
parseNew = (NewStack <$) . stripPrefix "deal into new stack"

parseCut :: String -> Maybe Op
parseCut = (Cut <$>) . (read <$>) . stripPrefix "cut "

example1 =
  [ "deal with increment 7"
  , "deal into new stack"
  , "deal into new stack"
  ]

example2 =
  [ "cut 6"
  , "deal with increment 7"
  , "deal into new stack"
  ]

example3 =
  [ "deal with increment 7"
  , "deal with increment 9"
  , "cut -2"
  ]

example4 =
  [ "deal into new stack"
  , "cut -2"
  , "deal with increment 7"
  , "cut 8"
  , "cut -4"
  , "deal with increment 7"
  , "cut 3"
  , "deal with increment 9"
  , "deal with increment 3"
  , "cut -1"
  ]

input =
  [ "cut -1353"
  , "deal with increment 63"
  , "cut -716"
  , "deal with increment 55"
  , "cut 1364"
  , "deal with increment 61"
  , "cut 1723"
  , "deal into new stack"
  , "deal with increment 51"
  , "cut 11"
  , "deal with increment 65"
  , "cut -6297"
  , "deal with increment 69"
  , "cut -3560"
  , "deal with increment 20"
  , "cut 1177"
  , "deal with increment 29"
  , "cut 6033"
  , "deal with increment 3"
  , "cut -3564"
  , "deal into new stack"
  , "cut 6447"
  , "deal into new stack"
  , "cut -4030"
  , "deal with increment 3"
  , "cut -6511"
  , "deal with increment 42"
  , "cut -8748"
  , "deal with increment 38"
  , "cut 5816"
  , "deal with increment 73"
  , "cut 9892"
  , "deal with increment 16"
  , "cut -9815"
  , "deal with increment 10"
  , "cut 673"
  , "deal with increment 12"
  , "cut 4518"
  , "deal with increment 52"
  , "cut 9464"
  , "deal with increment 68"
  , "cut 902"
  , "deal with increment 11"
  , "deal into new stack"
  , "deal with increment 45"
  , "cut -5167"
  , "deal with increment 68"
  , "deal into new stack"
  , "deal with increment 24"
  , "cut -8945"
  , "deal into new stack"
  , "deal with increment 36"
  , "cut 3195"
  , "deal with increment 52"
  , "cut -1494"
  , "deal with increment 11"
  , "cut -9658"
  , "deal into new stack"
  , "cut -4689"
  , "deal with increment 34"
  , "cut -9697"
  , "deal with increment 39"
  , "cut -6857"
  , "deal with increment 19"
  , "cut -6790"
  , "deal with increment 59"
  , "deal into new stack"
  , "deal with increment 52"
  , "cut -9354"
  , "deal with increment 71"
  , "cut 8815"
  , "deal with increment 2"
  , "cut 6618"
  , "deal with increment 47"
  , "cut -6746"
  , "deal into new stack"
  , "cut 1336"
  , "deal with increment 53"
  , "cut 6655"
  , "deal with increment 17"
  , "cut 8941"
  , "deal with increment 25"
  , "cut -3046"
  , "deal with increment 14"
  , "cut -7818"
  , "deal with increment 25"
  , "cut 4140"
  , "deal with increment 60"
  , "cut 6459"
  , "deal with increment 27"
  , "cut -6791"
  , "deal into new stack"
  , "cut 3821"
  , "deal with increment 13"
  , "cut 3157"
  , "deal with increment 13"
  , "cut 8524"
  , "deal into new stack"
  , "deal with increment 12"
  , "cut 5944"
  ]
-- function inverse(a, n)
--     t := 0;     newt := 1;    
--     r := n;     newr := a;    
--     while newr ≠ 0
--         quotient := r div newr
--         (t, newt) := (newt, t - quotient * newt) 
--         (r, newr) := (newr, r - quotient * newr)
--     if r > 1 then return "a is not invertible"
--     if t < 0 then t := t + n
--     return t


inverse :: Int -> Int -> Int
inverse a n = inverseR 0 1 n a
  where
    inverseR t t' r 0 = if r > 1 then error "not invertible" else t
    inverseR t t' r r' = let q = r `div` r' in inverseR t' (t - q*t') r' (r - q*r')





