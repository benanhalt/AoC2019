
import Data.Set as S

main :: IO ()
main = print $ biodiversity $ solve input S.empty

biodiversity :: Grid -> Integer
biodiversity grid =
  let
    bits = (\c -> if c == '#' then 1 else 0) <$> concat grid
  in
    sum $ zipWith (\b n -> b * (2^n)) bits [0..]

solve :: Grid -> S.Set Grid -> Grid
solve grid seen =
  if S.member grid seen
  then grid
  else solve (step grid) (S.insert grid seen)

type Pos = (Int, Int)

type Grid = [[Char]]

step :: Grid -> Grid
step grid = do
  y <- [0..4]
  pure $ do
    x <- [0..4]
    let n = neighbors grid (x,y)
    pure $ case look grid (x,y) of
      Just '#'
        | n == 1    -> '#'
        | otherwise -> '.'
      Just '.'
        | n == 1    -> '#'
        | n == 2    -> '#'
      otherwise     -> '.'

neighbors :: Grid -> Pos -> Int
neighbors grid (x,y) = sum $ do
  (x',y') <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
  pure $ case look grid (x',y') of
    Just '#' -> 1
    otherwise -> 0

look :: Grid -> Pos -> Maybe Char
look chars (x,y)
  | y >= 5 = Nothing
  | y < 0 = Nothing
  | x >= 5 = Nothing
  | x < 0 = Nothing
  | otherwise = Just $ chars !! y !! x


input :: Grid
input =
  [ "..#.."
  , "##..#"
  , "##..."
  , "#####"
  , ".#.##"
  ]
