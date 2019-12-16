{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Prelude hiding (Left, Right)
import Control.Monad (guard)
import Data.List (permutations, sortOn, foldl', nub, unfoldr, splitAt)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)
import qualified Data.Set as Set

data RoomContent = Empty | Wall | OxygenSys
  deriving (Show, Eq)

type Room = Map.Map (Integer, Integer) RoomContent

data Direction = North | South | West | East
  deriving (Eq, Show, Enum)

inputFromDir :: Direction -> Integer
inputFromDir = (+ 1) . toInteger . fromEnum

data DroidResponse = HitWall | Moved | Found
  deriving (Show, Eq, Enum)

outputToResp :: Integer -> DroidResponse
outputToResp = toEnum . fromInteger

data Droid = Droid
  { computer :: State
  , position :: (Integer, Integer)
  , direction :: Direction
  , room :: Room
  , target :: (Integer, Integer)
  }

droid0 :: Droid
droid0 = Droid
  { computer = state0 program []
  , room = Map.singleton (0,0) Empty
  , position = (0,0)
  , direction = North
  , target = (0,0)
  }

main :: IO ()
main = do
  droid <- runDroid droid0
  let r = last $ unfoldr removeDeadends $ room droid
  printRoom (0,0) $ r
  print $ nSteps r

nSteps :: Room -> Int
nSteps room = length $ filter ((== Empty) . snd) $ Map.toList room

removeDeadends :: Room -> Maybe (Room, Room)
removeDeadends room =
  if length deadends < 1 then Nothing else Just (room', room')
  where
    room' = foldr Map.delete room deadends
    deadends = filter isDeadend $ fmap fst $ filter ((== Empty) . snd) $ Map.toList room
    isDeadend (0,0) = False
    isDeadend p = (== 1) $ sum $ do
      d <- [North, South, East, West]
      let n = updatePosition d Moved p
      case Map.lookup n room of
        Just Empty -> pure 1
        Just OxygenSys -> pure 1
        _ -> pure 0



stepDroid :: Direction -> Droid -> Droid
stepDroid dir droid@Droid {computer, room, position, direction, target} = droid
  { computer = clearOutput computer'
  , room = room'
  , position = position'
  , direction = direction'
  , target = target'
  }
  where
    input = inputFromDir dir
    computer' = execState $ provideInput input computer
    resp = outputToResp $ head $ getOutput computer'
    room' = updateRoom position dir resp room
    position' = updatePosition dir resp position
    direction' = if position' == position then direction else dir
    target' = if resp == Found then position' else target

runDroid :: Droid -> IO Droid
runDroid droid@Droid {computer, room, position} = do
  putStrLn "\x1B[H\x1B[J"
  printRoom position room
  threadDelay 10000

  let dir = chooseDirection droid
  let droid'@Droid {position=position'} = stepDroid dir droid
  if (0,0) /= target droid'
    then pure droid'
    else runDroid $ droid'

chooseDirection :: Droid -> Direction
chooseDirection droid =
  case direction droid of
    dir | look (leftOf dir) droid /= Just Wall -> leftOf dir
        | look dir droid /= Just Wall -> dir
        | look (rightOf dir) droid /= Just Wall -> rightOf dir
        | otherwise -> leftOf $ leftOf dir

rightOf :: Direction -> Direction
rightOf North = East
rightOf East = South
rightOf South = West
rightOf West = North

leftOf :: Direction -> Direction
leftOf d = iterate rightOf d !! 3


look :: Direction -> Droid -> Maybe RoomContent
look dir Droid {room, position} =
  Map.lookup (updatePosition dir Moved position) room


updateRoom :: (Integer, Integer) -> Direction -> DroidResponse -> Room -> Room
updateRoom position dir resp = Map.insert position' content
   where
     position' = updatePosition dir Moved position
     content = case resp of
       Moved -> Empty
       HitWall -> Wall
       Found -> OxygenSys

updatePosition :: Direction -> DroidResponse -> (Integer, Integer) -> (Integer, Integer)
updatePosition dir HitWall position = position
updatePosition dir _ (x, y) = case dir of
  North -> (x, y-1)
  South -> (x, y+1)
  West -> (x-1, y)
  East -> (x+1, y)


printRoom :: (Integer, Integer) -> Room -> IO ()
printRoom position room =
  putStrLn $ unlines $ do
    y <- [(minimum ys) .. (maximum ys)]
    pure $ do
      x <- [(minimum xs) .. (maximum xs)]
      pure $ if position == (x,y)
        then 'D'
        else if (x,y) == (0,0)
        then 'S'
        else case Map.lookup (x,y) room of
               Just Wall -> '#'
               Just Empty -> '.'
               Just OxygenSys -> 'O'
               _ -> ' '
  where
    xs  = (fst . fst) <$> Map.toList room
    ys  = (snd . fst) <$> Map.toList room


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

program = [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1001,1034,0,1039,102,1,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1106,0,124,1001,1034,0,1039,1002,1036,1,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,1001,1035,0,1040,101,0,1038,1043,1002,1037,1,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,1001,1035,0,1040,101,0,1038,1043,102,1,1037,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,9,1032,1006,1032,165,1008,1040,39,1032,1006,1032,165,1102,2,1,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,72,1044,1105,1,224,1102,1,0,1044,1105,1,224,1006,1044,247,102,1,1039,1034,1002,1040,1,1035,1002,1041,1,1036,1002,1043,1,1038,1001,1042,0,1037,4,1044,1106,0,0,43,44,92,18,58,24,84,34,94,19,51,95,1,54,20,78,88,51,71,20,92,96,11,50,22,21,3,96,74,15,26,56,99,18,80,56,99,50,12,71,93,48,25,99,83,45,4,68,98,82,26,95,97,98,6,3,79,32,98,34,9,80,74,24,95,75,12,26,80,54,10,71,94,79,40,38,99,57,58,78,31,97,40,85,38,83,87,27,85,29,42,99,69,29,80,94,56,88,21,17,84,87,78,54,27,85,31,77,30,82,83,52,30,90,49,93,69,58,74,42,86,40,85,79,23,98,14,11,79,26,86,33,82,83,17,84,53,65,97,10,68,99,48,76,83,44,98,18,82,11,3,81,84,1,42,82,73,99,35,83,42,24,97,31,78,41,82,75,11,86,86,3,99,11,15,84,53,79,93,53,62,82,64,98,56,76,69,74,5,83,97,63,4,81,32,10,33,94,93,87,70,31,76,68,22,7,7,96,96,57,41,95,11,96,85,83,85,50,27,82,89,56,20,95,96,93,91,92,40,68,78,84,7,52,42,55,37,75,58,80,28,80,10,92,54,89,52,55,78,75,71,65,82,30,50,81,99,39,68,74,30,87,58,31,74,10,1,85,66,93,85,9,88,74,74,24,86,1,91,12,76,65,85,82,93,95,32,98,67,16,80,79,42,79,33,93,45,91,99,73,48,84,96,35,95,14,99,55,61,84,53,63,54,54,89,88,85,25,97,96,88,51,73,29,79,31,94,32,74,92,48,63,28,92,9,52,91,26,78,75,22,39,1,99,20,86,91,9,73,84,23,27,59,36,83,29,52,88,39,2,90,41,46,83,2,3,96,55,28,89,89,33,90,21,22,82,7,87,17,75,83,98,33,73,73,2,31,88,10,56,49,78,78,42,88,91,21,83,21,83,27,82,21,85,35,91,98,70,45,91,87,90,95,15,11,77,53,49,55,92,21,9,91,95,46,61,63,82,11,77,47,98,20,90,25,64,81,20,80,93,41,5,91,91,55,95,57,76,97,75,9,99,52,73,55,95,89,28,98,57,99,66,34,81,87,39,85,56,8,16,74,85,18,24,99,76,58,89,46,53,86,98,89,65,81,51,77,18,12,64,83,18,96,36,33,73,70,85,89,52,82,82,37,38,85,83,28,58,98,69,10,86,86,2,32,83,87,85,29,88,32,98,11,88,29,74,64,89,91,6,41,89,45,91,79,87,34,76,7,21,89,40,97,74,28,62,58,3,92,66,92,78,87,67,22,41,54,81,69,24,97,65,30,87,88,61,55,96,85,40,98,53,80,32,66,88,3,47,98,77,56,30,15,92,77,20,56,80,79,52,25,77,23,87,74,76,34,77,75,1,5,82,27,93,50,82,82,2,6,52,19,78,93,15,83,48,92,82,60,90,98,99,57,69,16,87,52,26,79,82,49,51,85,30,62,73,92,40,86,88,37,14,76,71,79,43,84,82,8,98,38,1,80,85,76,54,17,74,17,7,96,10,43,26,88,97,6,70,94,96,23,3,74,23,80,17,26,81,39,89,91,10,94,26,13,92,5,43,95,70,87,51,36,86,74,57,88,42,88,84,57,10,77,10,36,99,96,62,89,40,86,98,24,93,43,79,17,26,32,84,24,94,56,85,94,43,75,82,65,80,63,6,75,70,81,99,73,58,34,93,23,76,70,89,42,86,48,80,66,88,83,81,61,80,62,86,74,85,40,84,81,93,45,74,30,73,24,84,83,88,41,77,69,89,2,95,47,84,80,85,0,0,21,21,1,10,1,0,0,0,0,0,0]
