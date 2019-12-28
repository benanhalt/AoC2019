
import Control.Monad
import qualified Data.Map.Strict as M
import Data.List (iterate')

levels :: Int
levels = 200

main :: IO ()
main = do
  let result = (iterate' step $ initMap input) !! 200
  -- printMap result
  print $ sum $ M.elems result


printMap :: M.Map Cell Int -> IO ()
printMap m =
  forM_ [-levels .. levels] $ \l -> do
    print l
    forM_ [0 .. 4] $ \y -> do
      print $ [if 1 == m M.! (x,y,l) then '#' else '.' | x <- [0..4]]

initMap :: Grid -> M.Map Cell Int
initMap grid = M.fromList $ do
  l <- [-levels .. levels]
  y <- [0 .. 4]
  x <- [0 .. 4]
  let v = if l == 0
          then grid !! y !! x
          else 0
  pure ((x,y,l), v)

type Cell = (Int, Int, Int)

type Grid = [[Int]]

neighbors :: Cell -> [Cell]
neighbors (x,y,l)
  | (x,y) == (0,0) = [(x+1,y,l), (x,y+1,l), (2,1,l-1), (1,2,l-1)]
  | (x,y) == (4,4) = [(x-1,y,l), (x,y-1,l), (2,3,l-1), (3,2,l-1)]
  | (x,y) == (0,4) = [(x+1,y,l), (x,y-1,l), (1,2,l-1), (2,3,l-1)]
  | (x,y) == (4,0) = [(x-1,y,l), (x,y+1,l), (2,1,l-1), (3,2,l-1)]
  | x == 0 = [(x+1,y,l), (x,y+1,l), (x,y-1,l), (1,2,l-1)]
  | x == 4 = [(x-1,y,l), (x,y+1,l), (x,y-1,l), (3,2,l-1)]
  | y == 0 = [(x+1,y,l), (x,y+1,l), (x-1,y,l), (2,1,l-1)]
  | y == 4 = [(x+1,y,l), (x,y-1,l), (x-1,y,l), (2,3,l-1)]
  | (x,y) == (2,1) = [(x+1,y,l), (x-1,y,l), (x,y-1,l)] <> [(x,0,l+1) | x <- [0..4]]
  | (x,y) == (2,3) = [(x+1,y,l), (x-1,y,l), (x,y+1,l)] <> [(x,4,l+1) | x <- [0..4]]
  | (x,y) == (1,2) = [(x,y+1,l), (x,y-1,l), (x-1,y,l)] <> [(0,y,l+1) | y <- [0..4]]
  | (x,y) == (3,2) = [(x,y+1,l), (x,y-1,l), (x+1,y,l)] <> [(4,y,l+1) | y <- [0..4]]
  | otherwise = [(x+1,y,l), (x-1,y,l), (x,y+1,l), (x,y-1,l)]

step :: M.Map Cell Int -> M.Map Cell Int
step m = M.fromList $ do
  l <- [-levels .. levels]
  y <- [0 .. 4]
  x <- [0 .. 4]
  let n = sum $ (\c -> M.findWithDefault 0 c m) <$> neighbors (x,y,l)
  let v = if (x,y) == (2,2) then 0
          else case m M.! (x,y,l) of
                 1 | n == 1 -> 1
                   | otherwise -> 0
                 0 | n == 1 -> 1
                   | n == 2 -> 1
                 otherwise -> 0
  pure ((x,y,l), v)



char2Int :: Char -> Int
char2Int '#' = 1
char2Int _ = 0

input :: Grid
input = fmap char2Int <$>
  [ "..#.."
  , "##..#"
  , "##..."
  , "#####"
  , ".#.##"
  ]


example :: Grid
example = fmap char2Int <$>
  [ "....#"
  , "#..#."
  , "#.?##"
  , "..#.."
  , "#...."
  ]
