{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Control.Monad
import Control.DeepSeq
import Data.Char
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

data Grid = Grid
  { cols :: Int
  , rows :: Int
  , chars :: V.Vector Char
  }

data Queue a = Queue Int [a] [a] deriving Show

checkf :: Queue a -> Queue a
checkf (Queue i [] r) = Queue i (reverse r) []
checkf q = q

snoc :: a -> Queue a -> Queue a
snoc x (Queue i f r) = checkf $ Queue (i+1) f (x:r)

isEmpty :: Queue a -> Bool
isEmpty (Queue _ f _) = null f

empty :: Queue a
empty = Queue 0 [] []

front :: Queue a -> a
front (Queue _ f r) = head f

rest :: Queue a -> Queue a
rest (Queue i (x:xs) r) = checkf $ Queue (i-1) xs r

size :: Queue a -> Int
size (Queue i _ _) = i


newtype Pos = Pos {unPos :: (Int, Int)} deriving (Eq, Show, Ord)
data Portal = Portal {key :: String, loc :: Pos, dir :: PortalDir} deriving (Eq, Show)
data PortalDir = Inner | Outer deriving (Eq, Show)

data State = State
  { pos :: Pos
  , level :: Int
  , steps :: Int
  } deriving (Show, Eq, Ord)


main :: IO ()
main = do
  let grid = parseGrid input
  let portals = findPortals grid
  let Just entrance = find (("AA" ==) . key) portals
  let Just exit = find (("ZZ" ==) . key) portals
  let s0 = State {pos = loc entrance, level = 0, steps = 0}
  print $ search grid portals (loc exit) (s0 `snoc` empty) (rememberState s0 S.empty)

parseGrid :: [[Char]] -> Grid
parseGrid cs =
  let
    rows = length cs
    cols = length $ head cs
  in
    Grid {rows = rows, cols = cols, chars = V.concat $ V.fromList <$> cs}

rememberState :: State -> S.Set (Int, Pos) -> S.Set (Int, Pos)
rememberState State {level, pos} seen = S.insert (level, pos) seen

search :: Grid -> [Portal] -> Pos -> Queue State -> S.Set (Int, Pos) -> Int
search grid portals exit q seen =
  let
    s@State {pos, level, steps} = front q

    newStates = do
      let Pos (x,y) = pos
      pos' <- [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]
      guard $ not $ S.member (level, Pos pos') seen
      let s' = s {pos = Pos pos', steps=steps+1}
      guard $ isGood grid s'
      pure s'

    newStates' = newStates <> do
      thisP <- portals
      guard $ pos == loc thisP
      guard $ (level == 0) || (not $ key thisP `elem` ["AA", "ZZ"])
      dest <- portals
      guard $ dest /= thisP
      guard $ (key thisP) == (key dest)
      let level' = case dir thisP of
            Inner -> level + 1
            Outer -> level - 1
      guard $ level' >= 0
      guard $ not $ S.member (level', (loc dest)) seen
      pure $ s {pos = loc dest, steps=steps+1, level = level'}

    q' = foldl' (flip snoc) (rest q) newStates'
    seen' = foldl' (flip rememberState) seen newStates'

  in
    if (level, pos) == (0, exit) then steps
    else search grid portals exit q' seen'


isGood :: Grid -> State -> Bool
isGood grid State {pos} = look grid pos == Just '.'

topPortal :: Grid -> Pos -> Maybe Portal
topPortal grid p@(Pos (x,y)) =
  let
    up1 = look grid (Pos (x,y-1))
    up2 = look grid (Pos (x,y-2))
    c = look grid p
    d = if y == 2 then Outer else Inner
  in
    mayPortal d p $ catMaybes [c, up2, up1]

botPortal :: Grid -> Pos -> Maybe Portal
botPortal grid p@(Pos (x,y)) =
  let
    d1 = look grid (Pos (x,y+1))
    d2 = look grid (Pos (x,y+2))
    c = look grid p
    d = if y+3 == rows grid then Outer else Inner
  in
    mayPortal d p $ catMaybes [c, d1, d2]

lftPortal :: Grid -> Pos -> Maybe Portal
lftPortal grid p@(Pos (x,y)) =
  let
    l1 = look grid (Pos (x-1,y))
    l2 = look grid (Pos (x-2,y))
    c = look grid p
    d = if x == 2 then Outer else Inner
  in
    mayPortal d p $ catMaybes [c, l2, l1]

rhtPortal :: Grid -> Pos -> Maybe Portal
rhtPortal grid p@(Pos (x,y)) =
  let
    r1 = look grid (Pos (x+1,y))
    r2 = look grid (Pos (x+2,y))
    c = look grid p
    d = if x+3 == cols grid then Outer else Inner
  in
    mayPortal d p $ catMaybes [c, r1, r2]


mayPortal :: PortalDir -> Pos -> [Char] -> Maybe Portal
mayPortal d p  ['.', a, b] | isAlpha a && isAlpha b = Just $ Portal [a,b] p d
mayPortal _ _ _ = Nothing

findPortals :: Grid -> [Portal]
findPortals grid@Grid {rows, cols} = do
  y <- [0..rows-1]
  x <- [0..cols-1]
  p <- [topPortal, botPortal, lftPortal, rhtPortal]
  maybeToList $ p grid (Pos (x,y))


look :: Grid -> Pos -> Maybe Char
look Grid {rows, cols, chars} (Pos (x,y))
  | y >= rows = Nothing
  | y < 0 = Nothing
  | x >= cols = Nothing
  | x < 0 = Nothing
  | otherwise = Just $ chars V.! (x + y*cols)


example1 :: [[Char]]
example1 =
  [ "         A           "
  , "         A           "
  , "  #######.#########  "
  , "  #######.........#  "
  , "  #######.#######.#  "
  , "  #######.#######.#  "
  , "  #######.#######.#  "
  , "  #####  B    ###.#  "
  , "BC...##  C    ###.#  "
  , "  ##.##       ###.#  "
  , "  ##...DE  F  ###.#  "
  , "  #####    G  ###.#  "
  , "  #########.#####.#  "
  , "DE..#######...###.#  "
  , "  #.#########.###.#  "
  , "FG..#########.....#  "
  , "  ###########.#####  "
  , "             Z       "
  , "             Z       "
  ]


example2 :: [[Char]]
example2 =
  [ "                   A               "
  , "                   A               "
  , "  #################.#############  "
  , "  #.#...#...................#.#.#  "
  , "  #.#.#.###.###.###.#########.#.#  "
  , "  #.#.#.......#...#.....#.#.#...#  "
  , "  #.#########.###.#####.#.#.###.#  "
  , "  #.............#.#.....#.......#  "
  , "  ###.###########.###.#####.#.#.#  "
  , "  #.....#        A   C    #.#.#.#  "
  , "  #######        S   P    #####.#  "
  , "  #.#...#                 #......VT"
  , "  #.#.#.#                 #.#####  "
  , "  #...#.#               YN....#.#  "
  , "  #.###.#                 #####.#  "
  , "DI....#.#                 #.....#  "
  , "  #####.#                 #.###.#  "
  , "ZZ......#               QG....#..AS"
  , "  ###.###                 #######  "
  , "JO..#.#.#                 #.....#  "
  , "  #.#.#.#                 ###.#.#  "
  , "  #...#..DI             BU....#..LF"
  , "  #####.#                 #.#####  "
  , "YN......#               VT..#....QG"
  , "  #.###.#                 #.###.#  "
  , "  #.#...#                 #.....#  "
  , "  ###.###    J L     J    #.#.###  "
  , "  #.....#    O F     P    #.#...#  "
  , "  #.###.#####.#.#####.#####.###.#  "
  , "  #...#.#.#...#.....#.....#.#...#  "
  , "  #.#####.###.###.#.#.#########.#  "
  , "  #...#.#.....#...#.#.#.#.....#.#  "
  , "  #.###.#####.###.###.#.#.#######  "
  , "  #.#.........#...#.............#  "
  , "  #########.###.###.#############  "
  , "           B   J   C               "
  , "           U   P   P               "
  ]


example3 :: [[Char]]
example3 =
  [ "             Z L X W       C                 "
  , "             Z P Q B       K                 "
  , "  ###########.#.#.#.#######.###############  "
  , "  #...#.......#.#.......#.#.......#.#.#...#  "
  , "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
  , "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
  , "  #.###.#######.###.###.#.###.###.#.#######  "
  , "  #...#.......#.#...#...#.............#...#  "
  , "  #.#########.#######.#.#######.#######.###  "
  , "  #...#.#    F       R I       Z    #.#.#.#  "
  , "  #.###.#    D       E C       H    #.#.#.#  "
  , "  #.#...#                           #...#.#  "
  , "  #.###.#                           #.###.#  "
  , "  #.#....OA                       WB..#.#..ZH"
  , "  #.###.#                           #.#.#.#  "
  , "CJ......#                           #.....#  "
  , "  #######                           #######  "
  , "  #.#....CK                         #......IC"
  , "  #.###.#                           #.###.#  "
  , "  #.....#                           #...#.#  "
  , "  ###.###                           #.#.#.#  "
  , "XF....#.#                         RF..#.#.#  "
  , "  #####.#                           #######  "
  , "  #......CJ                       NM..#...#  "
  , "  ###.#.#                           #.###.#  "
  , "RE....#.#                           #......RF"
  , "  ###.###        X   X       L      #.#.#.#  "
  , "  #.....#        F   Q       P      #.#.#.#  "
  , "  ###.###########.###.#######.#########.###  "
  , "  #.....#...#.....#.......#...#.....#.#...#  "
  , "  #####.#.###.#######.#######.###.###.#.#.#  "
  , "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
  , "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
  , "  #.......#.....#.#...#...............#...#  "
  , "  #############.#.#.###.###################  "
  , "               A O F   N                     "
  , "               A A D   M                     "
  ]

input :: [[Char]]
input =
  [ "                                         V         C   O           F   O     A   K     H                                         "
  , "                                         Q         U   B           P   C     A   M     Y                                         "
  , "  #######################################.#########.###.###########.###.#####.###.#####.#######################################  "
  , "  #...#.....................#...........#.#...#.......#.......#.......#.#...........#.#.....#.#.......#.#...#.......#.#.#.#...#  "
  , "  ###.#####.#############.#############.#.#.#.#.#########.#.###.###.###.###.###.#####.###.###.#.#######.###.#.#######.#.#.#.###  "
  , "  #.#.....#.#.#.....#...........#.....#.....#.#.........#.#...#...#...#...#.#.....#.....#...#...#.........#...........#.#...#.#  "
  , "  #.#####.###.###.#######.###.#####.#.###.###.###.###.#######.#.#.#######.###.#.#####.###.###.#.#####.#####.###.#######.#.###.#  "
  , "  #...#.....#.#.#.....#...#.....#...#.......#...#.#.#.....#...#.#.....#.#...#.#.#.......#.....#.#...#.#.#.#...#.....#.#.......#  "
  , "  #.#######.#.#.#.#############.#.#.#.#######.#####.#.#####.#######.###.#.###.#.#####.#.#.#.#######.#.#.#.#.#########.#.#####.#  "
  , "  #...#.....#.....#.#.#...#.#.....#.#.#.......#.#.......#.....#.#.#.#.#...#...#.#.....#...#...#...............#.#...#.#.....#.#  "
  , "  #.###.#.#######.#.#.###.#.###########.#####.#.###.#####.#.###.#.#.#.#.#.###.#.###.#.#######.###.###.#.#.#####.#.###.###.#####  "
  , "  #...#.#.#...#...#.....#.......#.#.#.....#...#.....#.#.#.#...#.......#.#.#.#.#.#...#.#.....#.......#.#.#...#...#...#.......#.#  "
  , "  #.#####.#.###.#####.###.###.#.#.#.#.###.###.###.###.#.#####.#.#.#.###.###.#.#######.#.#.#######.#.###.#####.###.#######.###.#  "
  , "  #...#.....#.........#.....#.#.#...#.#...#.#...#.......#.....#.#.#.#.......#.#.#.......#.#.....#.#.#.#...#...........#.#...#.#  "
  , "  ###.#.#####.#.###.#.#########.###.#####.#.#####.###.###.#.###.#######.#####.#.#.#######.#.#####.###.#.#########.###.#.#.###.#  "
  , "  #...#.#...#.#.#.#.#.#.#.....#.#.#.#.#.....#...#...#.#...#.#...#.........#.....#.#...#.#.......#.#.#.#.#...#...#.#.#.#.#...#.#  "
  , "  ###.#.#.#######.###.#.#.#####.#.#.#.#.###.#.###.###.#.###.###.#######.#####.#####.###.###.#####.#.#.###.#.###.###.#.#.#.###.#  "
  , "  #...#.....#.#.#.#.......#.#.......#...#.......#.#...#...#.#...#.#.....#...#.....#.............#.......#.#.#.#.#...#...#.#...#  "
  , "  ###.###.###.#.#.###.#.#.#.#####.#.###.#.#.###.#.###.#.###.###.#.#####.###.#.#######.###########.#######.###.#.#.#.#.###.#.###  "
  , "  #.#.......#.#.......#.#.....#...#...#.#.#...#.#.#...#.#.#.#...#...#...#.#...#.#.#...........#.#...#...#...#...#.#...#...#...#  "
  , "  #.###.#####.#.#######.#.#.#####.###.#.#############.#.#.#####.###.###.#.###.#.#.#####.###.#.#.###.#.###.###.#######.###.#.###  "
  , "  #.#.........#.#.#.....#.#...#.#.#.#.......#...#...#.#.#.#.#.........#.#.#.#.#.#.#.....#...#.#...#.#...#...#.#.#...#.#.#.....#  "
  , "  #.###.###.#####.#####.###.###.###.###.#######.###.#.#.#.#.#########.#.#.#.#.#.#.#.###.#######.#####.###.###.#.#.#.#.#.#.#.#.#  "
  , "  #.......#.#.#.#.#...#...#.....#...........#.....#...#...#...........#...#.#.....#.#...........#.#.#.#...#...#.#.#.....#.#.#.#  "
  , "  #####.#####.#.#.###.#######.#########.#####.#.###.#####.#####.#######.###.#####.###.#######.###.#.#.###.#.#.#.###.#####.###.#  "
  , "  #.....#.....#.#.#.#.....#.............#...#.#.#.#...#...#.#.........#.....#.....#.......#.#.#.#.......#.#.#.#...#.#...#.#.#.#  "
  , "  #####.###.###.#.#.###.#######.#######.###.###.#.###.#.###.###.###.#######.#.#######.#####.#.#.#.###.###.###.#.#.#.###.###.#.#  "
  , "  #...#.#.......#.#.#.............#.#...#...#.......#.#...#.#.#.#.#...#.....#.......#.....#.........#.#.......#.#.#.....#.....#  "
  , "  #.###.#####.###.#.#.#.###.#######.###.#.#.###.#.###.###.#.#.#.#.###.#.#####.#####.#.###.#.###.#.#####.#.#######.#.#########.#  "
  , "  #.#.#...#.#.....#.#.#...#.#.#.#.....#.#.#.#.#.#.......#.....#.#.....#.#.....#...#.#...#.#.#...#...#...#...#...#.#.#.#.#.....#  "
  , "  #.#.#.###.#.#####.#.#######.#.###.###.#.#.#.#.#####.#.#.#.#####.#.###.###.#.#.#.#.###.#################.#.###.#.#.#.#.#.#.#.#  "
  , "  #...#.....#.......#.#...#...............#...#...#...#.#.#...#.#.#...#...#.#.#.#...#.......#...#.#.#...#.#...#.........#.#.#.#  "
  , "  ###.#.#####.###########.#########.#.#######.###.#########.###.#####.###.#####.###.#.#.#.###.###.#.#.###.#########.#########.#  "
  , "  #.....#.......#.#...#...#...#.#...#.#.......#.....#.........#.........#...#.....#.#.#.#.....#...#...#.......#.......#.......#  "
  , "  #.#####.#######.###.#.###.###.#######.#########.###########.###.#########.#####.###.###########.#.#####.#########.#########.#  "
  , "  #.#.#...#...#...#.#.#...#.......#    Y         K           Q   T         C     C   V        #.#...#.#...#.#.#...#...#...#...#  "
  , "  #.#.###.#.###.###.#.#.#######.###    G         F           J   M         U     O   Q        #.###.#.###.#.#.#.#####.#.#####.#  "
  , "  #.....#...#.#.#.....#.#.....#.#.#                                                           #.#.#.....#.....................#  "
  , "  ###.###.###.#.#.#.###.###.#.#.#.#                                                           #.#.#.#.###.###.#.###.#.#.###.###  "
  , "  #...#.......#.#.#.......#.#......HY                                                         #...#.#.#.....#.#.#...#.#.#.....#  "
  , "  #.#.#.###.###.#######.#######.###                                                           #.###.#.###.#######.#.#.###.#.###  "
  , "TM..#...#.......#.#.......#.#.....#                                                         FP..#...#.#.......#.#.#.#.#...#...#  "
  , "  #.#.#.#####.#.#.#####.###.#####.#                                                           #.#.###.#.###.#.#.###.###.#####.#  "
  , "  #.#.#...#...#.#.....#.#.#.....#.#                                                           #.....#...#.#.#.#...#.#.#.#...#..PI"
  , "  #####.###.#####.#####.#.###.#.#.#                                                           ###########.#####.#####.#####.###  "
  , "  #.#.#.#.....................#...#                                                           #.................#.........#....ML"
  , "  #.#.###########.###.###.###.#.###                                                           ###.#######.#.#.###.#.#######.#.#  "
  , "CO........#.........#...#.#.#.#.#.#                                                           #.....#.#...#.#.....#...#.....#.#  "
  , "  #####.#########.#######.#.#####.#                                                           #####.#####.#.#########.#.#.#.#.#  "
  , "  #.........#.........#...#.......#                                                         MV....#.....#.#.....#.....#.#.#.#.#  "
  , "  #.#####.#.###############.#######                                                           #.#.###.###.#.#.###.#####.#####.#  "
  , "  #...#.#.#.#.#...#...#.......#...#                                                         OB..#.......#.#.#.#.#.........#.#..UO"
  , "  ###.#######.#.#####.#####.###.#.#                                                           #################.#######.###.#.#  "
  , "  #.............................#..MX                                                         #.........#.........#.#.#...#...#  "
  , "  #########.###.###.#.#############                                                           #.#####.#########.#.#.#.#########  "
  , "SV....#.#.#...#.#...#.#.#...#...#.#                                                         VA..#.......#.......#.........#.#.#  "
  , "  ###.#.#.#############.#.#.#.#.#.#                                                           ###.#############.###.#######.#.#  "
  , "  #.....#...#.....#...#...#...#...#                                                           #.......#.#.#...#...#...#...#....CQ"
  , "  #####.#.###.###.###.###.#######.#                                                           #######.#.#.#.#####.###.###.###.#  "
  , "  #...#.......#.#.#.#.#.........#..KT                                                         #...#.................#.........#  "
  , "  #.###.#.#.###.###.#.#.#.#####.#.#                                                           #.#####################.#####.###  "
  , "  #...#.#.#.............#...#...#.#                                                           #...#.#...............#.#.#.#.#.#  "
  , "  #.#########.#.###.###.#.#########                                                           #.###.#.#######.#####.###.#.###.#  "
  , "  #.....#.#...#.#.....#.#...#.#.#.#                                                         XE......#.#...#...#.#...#.......#.#  "
  , "  #.#.###.###.###############.#.#.#                                                           #####.#.#########.#.###.#####.#.#  "
  , "KF..#.#.....#.#.........#...#.#...#                                                           #.....#.......#.#.......#.....#..QJ"
  , "  ###.###.#######.#######.###.###.#                                                           #.#######.#####.#.#######.#####.#  "
  , "  #...#...#.#.#.#.....#.....#.#.#..PA                                                         #.........#.....#.#.....#.......#  "
  , "  ###.#.#.#.#.#.#.#####.#.###.#.#.#                                                           ###########.#########.#########.#  "
  , "  #.#...#...............#.........#                                                           #.#.....#.....#.....#...#.....#.#  "
  , "  #.#.#########.#########.#.###.###                                                           #.#.#.#.#.###.#.#.###.#.###.#.###  "
  , "  #.#...#.....#.#.#.#.....#...#...#                                                           #...#.#.....#...#.....#.....#...#  "
  , "  #.#####.#.#.###.#.###############                                                           #.#######.#.###.###.###.#######.#  "
  , "  #...#.#.#.#.....#.......#...#.#.#                                                         ML....#.#...#.#.#...#.#.#.#...#....MX"
  , "  #.###.#.#####.###.#######.#.#.#.#                                                           #####.#######.#######.#####.#####  "
  , "  #.....#.#.#...#.....#.....#......KM                                                         #.#.............#.#.............#  "
  , "  #.#.###.#.#.###.###.#.#######.###                                                           #.#.###.#####.#.#.#.#######.###.#  "
  , "KV..#.....#.......#.....#.#...#...#                                                         CT..#.#.#...#.#.#.........#...#...#  "
  , "  #######################.#.#######                                                           #.#.###.###.###.#############.###  "
  , "  #.#.#.....#.#...................#                                                           #.#...#...#...#.#.....#.......#..FW"
  , "  #.#.#.#.###.###.#####.#.#.#.#.###                                                           #.###.#####.#######.#######.###.#  "
  , "YG..#...#.....#...#.#.#.#.#.#.#.#..OC                                                         #.......#.#.......#.#...#.#.....#  "
  , "  #.#.#######.#.###.#.#######.#.#.#                                                           #########.#####.###.###.#.#######  "
  , "  #.#.....#...#...........#...#...#                                                         FW................#...............#  "
  , "  #.###.###.#####.#########.#.###.#                                                           #.#.###.###.###.###.#.#.#.#.#.###  "
  , "ZZ......#.............#.....#.#.#.#                                                           #.#.#...#...#.......#.#.#.#.#....KT"
  , "  #.###.###.#####.#.#.#######.#.###            K   O       U           S       P       C      #.#.###.###.#######.###.#####.#.#  "
  , "  #.#.#.#.#.....#.#.#.....#.#.....#            V   E       O           V       I       Q      #.#.#...#.#.#...#.#...#...#...#.#  "
  , "  #.#.###.#.#.#.#.#.#######.#.###.#############.###.#######.###########.#######.#######.#############.#.#####.#.#.#.#.###.#.#.#  "
  , "  #...#.....#.#.#.#.....#.......#.#.....#.......#.......#.#.....#.#.....#.#.......#.#.....#...#.#.........#.......#.#.#...#.#.#  "
  , "  #.#.#.#.#.#########.#######.#######.#########.#.#.#.###.#####.#.###.#.#.###.#####.#.#####.#.#.#####.#######.###.#########.#.#  "
  , "  #.#.#.#.#...#.#...#...#.#.#.#.#.....#.....#...#.#.#.#.#.#.........#.#.#...#...#.#.........#...#...#.....#.#.#.........#...#.#  "
  , "  #.###.#.#####.#.###.###.#.#.#.#####.#####.###.#.#####.#.###.#.#######.#.###.###.###.#####.###.###.#.#####.#.#####.#.#.#.#.###  "
  , "  #.#...#...........#.#...........#.....#...#...#...#.#.#.....#.#...#.#.#...#...#...#...#.#...#.#.....#.#.........#.#.#.#.#...#  "
  , "  #####.###.###.###.###.###.#.#.#####.#####.#.###.###.#.#####.###.#.###.#.#.###.###.#.###.#########.#.#.#####.#####.#.###.###.#  "
  , "  #.....#.#.#.....#...#.#...#.#.#...............#.......#.#.....#.#...#.#.#...#...#.......#.#.......#...#...#.#.#.#.#...#.#.#.#  "
  , "  #.###.#.#####.#.#.#####.#.#.#######.#######.#####.###.#.#.###.#.###.#.#.###.#.#####.#.#.#.#######.#######.#.#.#.#######.#.#.#  "
  , "  #...#...#.....#.#.#.....#.#...#...#.#.#.........#.#.....#.#.#.#.#.....#...#.....#...#.#.....#.#.#.#.........#.#...#.......#.#  "
  , "  #.#####.#####.###.#.#.#.#######.###.#.#.#.###.#####.###.###.#.#.#########.###.#.#####.###.###.#.###.###.###.#.#.#######.#.#.#  "
  , "  #.#.....#.#.....#.#.#.#.#...#.......#...#...#.#...#...#.#.#.#.#.......#.....#.#.#.....#.#.#.#.#.....#.....#.........#.#.#.#.#  "
  , "  #.#.#.#.#.###.#.#########.#########.#.#######.#.#.###.###.#.#.#.#.###.#.#########.###.#.###.#.#####.#.#######.#.#.###.#####.#  "
  , "  #.#.#.#...#...#...#.#.#.#.#.#.......#.#.#.#...#.#.......#.....#.#.#.#.#...#.....#.#.....#.#.#.#.#...#.#.......#.#.#.#.#...#.#  "
  , "  #.#####.#.#.#.#####.#.#.#.#.###########.#.###.#.###.#######.#.#.#.#.#.#.#######.#####.###.#.#.#.#####.###.#.#.#####.#.#.#####  "
  , "  #.....#.#.#.#.#.#.#.#.........#...#...#.#.....#...#.#.......#.#.#.#...#...#.#.......#.............#.....#.#.#...........#.#.#  "
  , "  #.#####.###.#.#.#.#.#####.#.#####.###.#.###.#.#.#####.#.###.###.#.#.###.#.#.#.###.###.#######.#####.###########.#.###.###.#.#  "
  , "  #.#.....#.#.#.#.........#.#.#...#...#.....#.#.#.....#.#.#.#...#.#.#...#.#.#.#.#...#.....#.#.#.#...#...#.........#...#.......#  "
  , "  #.#####.#.#.#####.#.#######.#.#####.###.#.#.###.#.#####.#.#.#.###.###.#.###.###.#.#.#####.#.#####.#########.#.###.###.#.###.#  "
  , "  #.#.......#.#.#.#.#.#.#...#.#.....#...#.#.#...#.#.#.....#...#.#...#...#.#.....#.#.#.....#.....#.........#...#...#.#.#.#...#.#  "
  , "  #######.#####.#.###.#.###.#.#.#####.###.#####.#.#########.###.#####.###.#.#.###.#.###.###.#.###.#############.#####.#.#####.#  "
  , "  #.#.#...#.#.........#...........#.#.#...#.#...#.#.#...#...#.#...#.....#.#.#...#.#.........#.#.#.#.#...#...#.......#.....#.#.#  "
  , "  #.#.#.#.#.#########.#.#.###.#####.#.###.#.###.#.#.#.###.###.#######.###.#.###.#.#######.#####.#.#.#.#####.#.#.#####.#.###.#.#  "
  , "  #...#.#...#...#.......#.#.................#.#.#...#.....#.#.#.#...#...#.#...#.#.#.....#.#.#.....#.........#.#.....#.#.#.....#  "
  , "  #.#.#.#.###.#######.#.#.#.#.#.###.###.#.#.#.#.#.###.#.#.#.#.#.###.#.#.#.###.#.#.###.#.#.#.#####.#.#.#####.###.###.#####.#.#.#  "
  , "  #.#...#.#...#.......#.#.#.#.#.#.....#.#.#.#.#.#...#.#.#.......#.#.#.#.#.....#.#...#.#.#.#.........#...#.#.#.#...#...#.#.#.#.#  "
  , "  #####.###.###.#####.#.#######.###.#####.#.###.#.#########.#.###.#.###.#######.#.#.#.#.###.#.#.###.#.###.###.#.###.###.###.#.#  "
  , "  #.........#.#.#.....#.#.........#.#.....#...#.#...#...#...#.#.#...#.#...#.....#.#.#.#.#...#.#.#.#.#.........#.#.........#.#.#  "
  , "  #.###.#####.#####.#.###.###############.###.#.###.###.#.#####.#.#.#.#.#####.#####.#.#.#.#.#####.#.###.#####.###.###.#######.#  "
  , "  #.#.....#.........#.#...#.................#...#.......#.......#.#...#.#.........#...#.#.#.......#...#.#...#.#.#...#...#...#.#  "
  , "  #.#.#.#####.#.###.###.#.###.###.#.#.#.###.#####.###.###.#.#######.###.#######.#######.#.#.#######.#####.#####.#####.#.#.###.#  "
  , "  #.#.#.....#.#.#...#...#.#...#...#.#.#.#.......#...#.#...#.....#.........#.....#.........#.....#...................#.#.#.....#  "
  , "  #############################################.###.###########.#####.#####.#######.###########################################  "
  , "                                               X   V           C     O     M       P                                             "
  , "                                               E   A           T     E     V       A                                             "
  ]