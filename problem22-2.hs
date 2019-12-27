{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List
import Data.Maybe
import Control.Applicative

main :: IO ()
main = do
  putStrLn "Part 1:"
  print $ findCard 2019 $ applySeq 10007 $ parseAll input
  putStrLn "Part 2:"
  let result1 = applySeq 119315717514047 $ parseAll input
  -- print result1
  print $ cardAt 2020 $ iterateShuffle 101741582076661 result1


data Op
  = NewStack
  | Cut Integer
  | Increment Integer
  deriving Show

type Shuffle = (Integer, (Integer, Integer))

nullShuffle :: Integer -> Shuffle
nullShuffle n = (n, (1, 0))

composeShuffles :: Shuffle -> Shuffle -> Shuffle
composeShuffles (n, (s, t)) (m, (s', t')) =
  s `seq` t `seq` s' `seq` t' `seq`
  if m /= n then error "incompatible"
  else (n, ((s'*s) `mod` n, (s'*t + t') `mod` n))

-- j = si + t
-- j' = s'j + t'
-- j' = s'(si + t) + t'
-- j' = s'si + s't + t'

iterateShuffle :: Integer -> Shuffle -> Shuffle
iterateShuffle n (m, (s, t)) = (m, (sn, t * x `mod` m))
  where
    sn = modPow s n m
    x = ((sn - 1) * inverse (s - 1) m) `mod` m


-- i1 = si + t
-- i2 = s*(si + t) + t = s*s*i + s*t + t
-- i3 = s*(s*s*i + s*t + t) + t = s*s*s*i + s*s*t + s*t + t
-- i4 = s^4i + (s^3 + s^2 + s + 1)t
-- in = s^n i + (s^n-1 + ... + 1)t
---   = s^n i + (s^n - 1) t / (s - 1)


-- s(s^n + ... + 1) = s^n+1 + s^n + ... + 1 - 1
-- (s - 1) (s^n + ... + 1) = s^n+1 - 1
-- s^n + ... + 1 =  s^n+1 - 1 / s - 1

toList :: Shuffle -> [Integer]
toList d@(n, _)  = flip cardAt d <$> [0..n-1]

findCard :: Integer -> Shuffle -> Maybe Int
findCard j d = elemIndex j $ toList d

cardAt :: Integer -> Shuffle -> Integer
cardAt i (n, (s, t)) = (s * i + t) `mod` n

-- inc a -> j = inverse(a, 10) * i mod 10 -> j = inverse(a, 10) * (si + t) mod 10
-- j = (inverse(a, 10) * s)i + (inverse(a, 10) * t) `mod` 10

-- new -> j = 9-i mod 10 -> j = 9 - (si + t) mod 10
-- j = (-s)i + (9-t) mod 10

-- cut a -> j = i + a mod 10 -> j = si + t + a mod 10
-- j = si + (t + a) mod 10

applyOp :: Op -> Integer -> Shuffle
applyOp NewStack n = (n, (-1, n-1))
applyOp (Cut a) n = (n, (1, a))
applyOp (Increment a) n = (n, (inverse a n, 0))

applySeq :: Integer -> [Op] -> Shuffle
applySeq n ops = foldl' composeShuffles (nullShuffle n) $ flip applyOp n <$> reverse ops


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


inverse :: Integer -> Integer -> Integer
inverse a n = inverseR 0 1 n a
  where
    inverseR t t' r 0 = if r > 1 then error "not invertible" else t
    inverseR t t' r r' = let q = r `div` r' in inverseR t' (t - q*t') r' (r - q*r')

-- function modular_pow(base, exponent, modulus) is
--     if modulus = 1 then
--         return 0
--     Assert :: (modulus - 1) * (modulus - 1) does not overflow base
--     result := 1
--     base := base mod modulus
--     while exponent > 0 do
--         if (exponent mod 2 == 1) then
--             result := (result * base) mod modulus
--         exponent := exponent >> 1
--         base := (base * base) mod modulus
--     return result

modPow :: Integer -> Integer -> Integer -> Integer
modPow base exponent m =
  if m == 1
  then 0
  else doIt (base `mod` m) exponent 1
  where
    doIt _ 0 result = result
    doIt b x r = doIt ((b*b) `mod` m) (x `div` 2) (if x `mod` 2 == 1 then (r*b) `mod` m else r)

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




