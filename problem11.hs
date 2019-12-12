{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Prelude hiding (Left, Right)
import Control.Monad (forM_)
import Data.List (permutations, sortOn, foldl', nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

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


type Panels = Map.Map (Int, Int) Int

ppPanels :: Panels -> String
ppPanels panels = unlines $ do
  y :: Int <- [(minimum ys) .. (maximum ys)]
  pure $ do
    x :: Int <- [(minimum xs) .. (maximum xs)]
    pure $ case Map.lookup (x,y) panels of
      Just 1 -> '#'
      _ -> ' '
  where
    xs :: [Int] = (fst . fst) <$> Map.toList panels
    ys :: [Int] = (snd . fst) <$> Map.toList panels

data Robot = Robot
  { computer :: State
  , position :: (Int, Int)
  , heading :: Direction
  , panels :: Panels
  , visited :: [(Int, Int)]
  } deriving (Show)

robot0 :: [Integer] -> Robot
robot0 program = Robot
  { computer = state0 program []
  , position = (0,0)
  , heading = Up
  , panels = Map.insert (0,0) 1 Map.empty
  , visited = [(0,0)]
  }

data Direction = Up | Right | Down | Left deriving (Show)

robotStep :: Robot -> Robot
robotStep robot@Robot {computer} =
  handleMovement $ handlePainting $ robot {computer = computer'}
  where
    computer' = execState $ clearOutput $
      case status computer of
        WaitingForInput -> provideInput (toInteger $ getPanelColor robot) computer
        otherwise -> computer

handlePainting :: Robot -> Robot
handlePainting robot@Robot {computer, position, panels} =
  case getOutput computer of
    [paintCode, _] -> robot {panels = paintPanel position (fromInteger paintCode) panels}
    otherwise -> robot

handleMovement :: Robot -> Robot
handleMovement robot@Robot {computer, heading, position, visited} =
  case getOutput computer of
    [_, dirCode] ->
      let
        heading' = adjustHeading dirCode heading
        position' = updatePosition heading' position
      in robot {heading = heading', position = position', visited = position':visited}

    otherwise -> robot

adjustHeading :: Integer -> Direction -> Direction
adjustHeading 0 Up = Left
adjustHeading 0 Left = Down
adjustHeading 0 Down = Right
adjustHeading 0 Right = Up
adjustHeading 1 Up = Right
adjustHeading 1 Right = Down
adjustHeading 1 Down = Left
adjustHeading 1 Left = Up

updatePosition :: Direction -> (Int, Int) -> (Int, Int)
updatePosition Up (x, y) = (x, y-1)
updatePosition Down (x, y) = (x, y+1)
updatePosition Left (x, y) = (x-1, y)
updatePosition Right (x, y) = (x+1, y)

paintPanel :: (Int, Int) -> Int -> Panels -> Panels
paintPanel = Map.insert

getPanelColor :: Robot -> Int
getPanelColor Robot {position, panels} =
  fromMaybe 0 $ Map.lookup position panels

isFinished :: Robot -> Bool
isFinished Robot{computer=State {status}} = status == Halted

runRobot :: [Integer] -> Robot
runRobot program = head $ snd $ break isFinished $ iterate robotStep $ robot0 program

main :: IO ()
main = putStrLn $ ppPanels $ panels $ runRobot program


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

program = [3,8,1005,8,330,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,29,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,51,1,1103,2,10,1006,0,94,1006,0,11,1,1106,13,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,87,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,109,2,1105,5,10,2,103,16,10,1,1103,12,10,2,105,2,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,146,1006,0,49,2,1,12,10,2,1006,6,10,1,1101,4,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,183,1,6,9,10,1006,0,32,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,213,2,1101,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,239,1006,0,47,1006,0,4,2,6,0,10,1006,0,58,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,274,2,1005,14,10,1006,0,17,1,104,20,10,1006,0,28,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,309,101,1,9,9,1007,9,928,10,1005,10,15,99,109,652,104,0,104,1,21101,0,937263411860,1,21102,347,1,0,1105,1,451,21101,932440724376,0,1,21102,1,358,0,1105,1,451,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,29015167015,1,21101,0,405,0,1106,0,451,21102,1,3422723163,1,21101,0,416,0,1106,0,451,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,868389376360,1,21101,0,439,0,1105,1,451,21102,825544712960,1,1,21102,1,450,0,1106,0,451,99,109,2,21201,-1,0,1,21101,0,40,2,21102,482,1,3,21102,1,472,0,1106,0,515,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,477,478,493,4,0,1001,477,1,477,108,4,477,10,1006,10,509,1101,0,0,477,109,-2,2106,0,0,0,109,4,2101,0,-1,514,1207,-3,0,10,1006,10,532,21102,1,0,-3,22101,0,-3,1,22102,1,-2,2,21102,1,1,3,21101,551,0,0,1106,0,556,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,579,2207,-4,-2,10,1006,10,579,22102,1,-4,-4,1106,0,647,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,598,0,1106,0,556,22101,0,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,617,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,639,21201,-1,0,1,21102,639,1,0,105,1,514,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]
