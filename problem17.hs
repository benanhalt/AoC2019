{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Prelude hiding (Left, Right)
import Control.Monad (guard)
import Data.List (permutations, sortOn, foldl', nub, unfoldr, splitAt, intercalate)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Char (chr, ord)
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)

type Grid = [[Char]]

data Dir = F | L | R deriving (Eq, Show, Ord)

newtype Pos = Pos {unPos :: (Int, Int)} deriving (Eq, Show)
newtype Step = Step {unStep :: (Int, Int)} deriving (Eq, Show)




main :: IO ()
main = do
  let map = (chr . fromInteger) <$> (getOutput $ execState $ state0 program [])
  putStrLn $ map
  let grid = map2grid map
  let xs = findIntersections grid
  print $ xs
  putStrLn "\nPart 1:"
  print $ sum $ (uncurry (*) . unPos) <$> xs

  let [bot] = findBot grid
  print $ bot
  let path = unfoldr (fmap (\(d,p,dp) -> (d, (p,dp))) . stepBot grid) (bot, Step (0,-1))
  print $ path
  print $ path2code path

-- By Inspection

  let a = "L,12,R,4,R,4,L,6"
  let b = "L,12,R,4,R,4,R,12"
  let c = "L,10,L,6,R,4"
  let main = "A,B,A,C,A,B,C,B,C,A"

  let program' = 2 : drop 1 program
  let inputs = (toInteger . ord) <$> intercalate "\n" [main, a, b, c, "n", "\n"]

  let outputs = getOutput $ execState $ state0 program' inputs
  putStrLn $ (chr . fromInteger) <$> (init outputs)

  putStrLn "\nPart 2:"
  print $ last outputs

path2code :: [Dir] -> String
path2code path = intercalate "," $ unfoldr generate path
  where
    generate [] = Nothing
    generate p@(F : _) = Just (show $ length fs, rest)
      where (fs, rest) = span (== F) p
    generate (lr : rest) = Just (show lr, rest)

map2grid :: String -> Grid
map2grid = init . lines

findIntersections :: Grid -> [Pos]
findIntersections grid = do
  y <- [1 .. (length grid - 2)]
  x <- [1 .. (length (head grid) - 2)]
  guard $ all (== '#')
    [ grid !! y !! x
    , grid !! (y-1) !! x
    , grid !! (y+1) !! x
    , grid !! y !! (x-1)
    , grid !! y !! (x+1)
    ]
  pure $ Pos (x, y)


findBot :: Grid -> [Pos]
findBot grid = do
  y <- [1 .. (length grid - 2)]
  x <- [1 .. (length (head grid) - 2)]
  guard $ elem (grid !! y !! x) ['<', '>', '^', 'v']
  pure $ Pos (x, y)

look :: Grid -> Pos -> Maybe Char
look grid (Pos (x,y))
  | y >= length grid = Nothing
  | y < 0 = Nothing
  | x >= length (head grid) = Nothing
  | x < 0 = Nothing
  | otherwise = Just $ grid !! y !! x

updateDir :: Step ->  Dir -> Step
updateDir (Step (dx, dy)) dir = Step $ case dir of
  F -> (dx, dy)
  L -> (dy, -dx)
  R -> (-dy, dx)

updatePos :: Pos -> Step -> Pos
updatePos (Pos (x,y)) (Step (dx, dy)) = Pos (x+dx, y+dy)

stepBot :: Grid -> (Pos, Step) -> Maybe (Dir, Pos, Step)
stepBot grid (p, dp) =
  listToMaybe $ do
    d <- [F, L, R]
    let dp' = updateDir dp d
    let p' = updatePos p dp'
    guard $ look grid p' == Just '#'
    pure (d, if d == F then p' else p, dp')

type Mem = Map.Map Integer Integer

(!!!) :: Mem -> Integer -> Integer
mem !!! i = fromMaybe 0 $ Map.lookup i mem

newtype OpCode = OpCode Integer
newtype ModeCode = ModeCode Integer

data Status = Running | Halted | WaitingForInput | Error
  deriving (Eq, Show)

data State = State
  { pc :: Integer
  , mem :: Mem
  , relBase :: Integer
  , status :: Status
  , inputs :: [Integer]
  , outputs :: [Integer]
  } deriving (Show)


state0 :: [Integer] -> [Integer] -> State
state0 program inputs = State
    { pc = 0
    , mem = Map.fromList $ zip [0 ..] program
    , relBase = 0
    , status = Running
    , inputs = inputs
    , outputs = []
    }


execState :: State -> State
execState state0 = stateF
  where
    (running, notRunning) = span isRunning $ iterate step state0
    stateF = head notRunning

isRunning :: State -> Bool
isRunning State {status} = status == Running

isWaitingForInput :: State -> Bool
isWaitingForInput State {status} = status == WaitingForInput

provideInput :: Integer -> State -> State
provideInput i s@State {inputs, status} = s {inputs=inputs <> [i], status=status'}
  where
    status' = case status of
      WaitingForInput -> Running
      _ -> status

getOutput :: State -> [Integer]
getOutput State {outputs} = reverse outputs

clearOutput :: State -> State
clearOutput s = s {outputs = []}

step :: State -> State
step s@State {pc, mem, status} =
  case status of
    Running ->
      case decodeOp $ mem !!! pc of
        (OpCode 1, modes) -> addOp modes s
        (OpCode 2, modes) -> multOp modes s
        (OpCode 3, modes) -> inOp modes s
        (OpCode 4, modes) -> outOp modes s
        (OpCode 5, modes) -> jmpIfTrueOp modes s
        (OpCode 6, modes) -> jmpIfFalseOp modes s
        (OpCode 7, modes) -> ltOp modes s
        (OpCode 8, modes) -> eqOp modes s
        (OpCode 9, modes) -> adjRelBaseOp modes s
        (OpCode 99, modes) -> s {status = Halted}
        _ -> s {status = Error}
    _ -> s

decodeOp :: Integer -> (OpCode, [ModeCode])
decodeOp i = (OpCode opCode, ModeCode <$> modeCodes)
  where
    opCode = i `rem` 100
    modeCodes = fmap (`rem` 10) $ iterate (`div` 10) $ i `div` 100

readOperand :: [ModeCode] -> Int -> State -> Integer
readOperand modes i State {pc, mem, relBase} =
  case modes !! i of
    ModeCode 0 -> mem !!! val  -- position mode
    ModeCode 1 -> val          -- immediate mode
    ModeCode 2 -> mem !!! (val + relBase) -- relative mode
  where
    val = mem !!! (pc + 1 + toInteger i)

writeOperand :: [ModeCode] -> Int -> Integer -> State -> Mem
writeOperand modes i val s@State {pc, mem, relBase} = Map.insert k val mem
  where
    j = mem !!! (pc + 1 + toInteger i)
    k = case modes !! i of
      ModeCode 0 -> j           -- position mode
      ModeCode 2 -> j + relBase -- relative mode

addOp :: [ModeCode] -> State -> State
addOp modes s@State {pc, mem} = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    mem' = writeOperand modes 2 (a + b) s

multOp :: [ModeCode] -> State -> State
multOp modes s@State {pc, mem} = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    mem' = writeOperand modes 2 (a * b) s

inOp :: [ModeCode] -> State -> State
inOp modes s@State {pc, mem, inputs} =
  case inputs of
    [] -> s {status = WaitingForInput}
    _ -> s {pc=pc + 2, mem=mem', inputs=tail inputs}
      where
        input = head inputs
        mem' = writeOperand modes 0 input s

outOp :: [ModeCode] -> State -> State
outOp modes s@State {pc, mem, outputs} = s {pc=pc + 2, outputs=a : outputs}
  where
    a = readOperand modes 0 s

jmpIfTrueOp :: [ModeCode] -> State -> State
jmpIfTrueOp modes s@State {pc, mem} = s {pc=pc'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    pc' = if a == 0 then pc + 3 else b

jmpIfFalseOp :: [ModeCode] -> State -> State
jmpIfFalseOp modes s@State {pc, mem} = s {pc=pc'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    pc' = if a == 0 then b else pc + 3

ltOp :: [ModeCode] -> State -> State
ltOp modes s@State {pc, mem} = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    lt = if a < b then 1 else 0
    mem' = writeOperand modes 2 lt s

eqOp :: [ModeCode] -> State -> State
eqOp modes s@State {pc, mem} = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    eq = if a == b then 1 else 0
    mem' = writeOperand modes 2 eq s

adjRelBaseOp :: [ModeCode] -> State -> State
adjRelBaseOp modes s@State {pc, relBase} = s {pc=pc + 2, relBase=relBase + a}
  where
    a = readOperand modes 0 s

program = [1,330,331,332,109,2780,1102,1,1182,16,1102,1417,1,24,101,0,0,570,1006,570,36,101,0,571,0,1001,570,-1,570,1001,24,1,24,1105,1,18,1008,571,0,571,1001,16,1,16,1008,16,1417,570,1006,570,14,21101,58,0,0,1105,1,786,1006,332,62,99,21102,333,1,1,21101,73,0,0,1106,0,579,1101,0,0,572,1101,0,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,102,1,574,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1106,0,81,21102,340,1,1,1106,0,177,21102,1,477,1,1106,0,177,21102,514,1,1,21101,0,176,0,1106,0,579,99,21101,184,0,0,1106,0,579,4,574,104,10,99,1007,573,22,570,1006,570,165,102,1,572,1182,21101,375,0,1,21101,211,0,0,1105,1,579,21101,1182,11,1,21102,222,1,0,1105,1,979,21101,0,388,1,21102,233,1,0,1106,0,579,21101,1182,22,1,21102,244,1,0,1105,1,979,21101,0,401,1,21102,1,255,0,1106,0,579,21101,1182,33,1,21101,0,266,0,1105,1,979,21101,0,414,1,21102,1,277,0,1105,1,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,0,1182,1,21101,313,0,0,1105,1,622,1005,575,327,1101,1,0,575,21101,327,0,0,1106,0,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,20,18,0,109,4,2102,1,-3,587,20102,1,0,-1,22101,1,-3,-3,21101,0,0,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1106,0,597,109,-4,2105,1,0,109,5,1202,-4,1,629,21002,0,1,-2,22101,1,-4,-4,21102,0,1,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,653,20102,1,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21101,0,702,0,1105,1,786,21201,-1,-1,-1,1105,1,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21102,1,731,0,1106,0,786,1106,0,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21101,0,756,0,1106,0,786,1106,0,774,21202,-1,-11,1,22101,1182,1,1,21101,0,774,0,1106,0,622,21201,-3,1,-3,1105,1,640,109,-5,2105,1,0,109,7,1005,575,802,21002,576,1,-6,20101,0,577,-5,1106,0,814,21102,1,0,-1,21101,0,0,-5,21101,0,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,47,-3,22201,-6,-3,-3,22101,1417,-3,-3,2102,1,-3,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21101,1,0,-1,1105,1,924,1205,-2,873,21102,35,1,-4,1106,0,924,1201,-3,0,878,1008,0,1,570,1006,570,916,1001,374,1,374,1201,-3,0,895,1102,2,1,0,2102,1,-3,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,922,20102,1,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,47,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,29,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1101,0,1,575,21102,973,1,0,1105,1,786,99,109,-7,2105,1,0,109,6,21101,0,0,-4,21102,1,0,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1106,0,1041,21101,0,-4,-2,1105,1,1041,21101,-5,0,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,2102,1,-2,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2101,0,-2,0,1106,0,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1105,1,989,21101,439,0,1,1105,1,1150,21101,477,0,1,1106,0,1150,21101,0,514,1,21101,1149,0,0,1106,0,579,99,21101,1157,0,0,1106,0,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1201,-5,0,1176,1202,-4,1,0,109,-6,2106,0,0,36,5,42,1,3,1,42,1,3,1,42,1,3,1,6,5,23,13,6,1,3,1,23,1,7,1,10,1,3,1,23,1,7,1,10,1,3,1,23,1,7,1,10,13,13,5,5,1,14,1,7,1,13,1,1,1,1,1,5,1,14,1,7,1,13,1,1,1,1,1,1,11,8,1,7,1,13,1,1,1,1,1,1,1,3,1,5,1,8,1,7,1,13,1,1,1,1,7,5,1,8,1,7,1,13,1,1,1,3,1,9,1,8,1,3,5,13,1,1,7,7,1,8,1,3,1,17,1,5,1,1,1,7,1,8,13,9,1,1,5,1,1,7,1,12,1,7,1,9,1,1,1,5,1,7,1,12,13,5,1,1,1,5,13,16,1,9,1,1,1,13,1,3,1,12,5,5,5,1,1,13,1,3,1,12,1,9,1,5,1,13,1,3,1,12,1,9,1,5,1,13,5,12,1,9,1,5,1,30,1,5,7,3,1,30,1,9,1,1,1,3,1,30,11,1,1,3,1,42,1,3,1,42,5,18]
