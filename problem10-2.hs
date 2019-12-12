import Control.Monad (guard)
import Data.List (nub, sortOn, groupBy, transpose)
import Data.Function (on)


astroidPositionsInLine :: String -> [Int]
astroidPositionsInLine line = do
  (x, c) <- zip [0..] line
  guard (c == '#')
  pure x

astroidPositions :: [String] -> [(Int, Int)]
astroidPositions astrMap =
  concat $ zipWith (\y line -> (\x -> (x,y)) <$> astroidPositionsInLine line) [0..] astrMap

vectorFrom :: (Int, Int) -> (Int, Int) -> (Int, Int)
vectorFrom (x0, y0) (x1, y1) = (x1-x0, y1-y0)

toLowestTerms :: (Int, Int) -> (Int, Int)
toLowestTerms (x, y) = (x `div` d, y `div` d)
  where d = gcd x y

visibleFrom :: (Int, Int) -> [(Int, Int)] -> Int
visibleFrom p qs = length $ nub $ toLowestTerms <$> vectorFrom p <$> (filter (/= p) qs)

mag2 :: (Int, Int) -> Int
mag2 (x, y) = x*x + y*y

quadrantAndAngle :: (Int, Int) -> (Int, Double)
quadrantAndAngle (x, y)
  | x > 0 && y >= 0 = (2, atan $ (fromIntegral y) / (fromIntegral x))
  | x <= 0 && y > 0 = (3, atan $ (fromIntegral $ - x) / (fromIntegral y))
  | x < 0 && y <= 0 = (4, atan $ (fromIntegral $ - y) / (fromIntegral $ - x))
  | otherwise = (1, atan $ (fromIntegral x) / (fromIntegral $ - y))

main :: IO ()
main = print $ inOrderOfDestruction !! 199
  where
    astroids = astroidPositions input
    station = last $ sortOn (\p -> visibleFrom p astroids) astroids
    inOrderOfDestruction =
      concat
      $ transpose
      $ fmap (sortOn (mag2 . vectorFrom station))
      $ sortOn (quadrantAndAngle . vectorFrom station . head)
      $ groupBy ((==) `on` (toLowestTerms. vectorFrom station))
      $ sortOn (toLowestTerms . vectorFrom station)
      $ filter (/= station) astroids

input :: [String]
input =
  [ "#.#.##..#.###...##.#....##....###"
  , "...#..#.#.##.....#..##.#...###..#"
  , "####...#..#.##...#.##..####..#.#."
  , "..#.#..#...#..####.##....#..####."
  , "....##...#.##...#.#.#...#.#..##.."
  , ".#....#.##.#.##......#..#..#..#.."
  , ".#.......#.....#.....#...###....."
  , "#.#.#.##..#.#...###.#.###....#..#"
  , "#.#..........##..###.......#...##"
  , "#.#.........##...##.#.##..####..#"
  , "###.#..#####...#..#.#...#..#.#..."
  , ".##.#.##.........####.#.#...##..."
  , "..##...#..###.....#.#...#.#..#.##"
  , ".#...#.....#....##...##...###...#"
  , "###...#..#....#............#....."
  , ".#####.#......#.......#.#.##..#.#"
  , "#.#......#.#.#.#.......##..##..##"
  , ".#.##...##..#..##...##...##.....#"
  , "#.#...#.#.#.#.#..#...#...##...#.#"
  , "##.#..#....#..##.#.#....#.##...##"
  , "...###.#.#.......#.#..#..#...#.##"
  , ".....##......#.....#..###.....##."
  , "........##..#.#........##.......#"
  , "#.##.##...##..###.#....#....###.#"
  , "..##.##....##.#..#.##..#.....#..."
  , ".#.#....##..###.#...##.#.#.#..#.."
  , "..#..##.##.#.##....#...#........."
  , "#...#.#.#....#.......#.#...#..#.#"
  , "...###.##.#...#..#...##...##....#"
  , "...#..#.#.#..#####...#.#...####.#"
  , "##.#...#..##..#..###.#..........#"
  , "..........#..##..#..###...#..#..."
  , ".#.##...#....##.....#.#...##...##"
  ]

input' =
  [".#..##.###...#######"
  ,"##.############..##."
  ,".#.######.########.#"
  ,".###.#######.####.#."
  ,"#####.##.#.##.###.##"
  ,"..#####..#.#########"
  ,"####################"
  ,"#.####....###.#.#.##"
  ,"##.#################"
  ,"#####.##.###..####.."
  ,"..######..##.#######"
  ,"####.##.####...##..#"
  ,".#####..#.######.###"
  ,"##...#.##########..."
  ,"#.##########.#######"
  ,".####.#.###.###.#.##"
  ,"....##.##.###..#####"
  ,".#.#.###########.###"
  ,"#.#.#.#####.####.###"
  ,"###.##.####.##.#..##"]
