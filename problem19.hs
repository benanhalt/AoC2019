{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Prelude hiding (Left, Right)
import Control.Monad (forM_, guard)
import Data.List (permutations, sortOn, foldl', nub, unfoldr, splitAt, intercalate)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Char (chr, ord)
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let results = do
        y <- [0 .. 49]
        x <- [0 .. 49]
        getOutput $ execState $ state0 program [x, y]

  putStrLn "Part 1:"
  print $ sum results

  -- forM_ results $ print

  let (x,y) = head $ solve 100
  print (x,y)
  putStrLn "Part 2:"
  print $ x*10000 + y


inBeam :: Integer -> Integer -> Bool
inBeam x y
  | x < 0 = False
  | y < 0 = False
  | otherwise = (getOutput $ execState $ state0 program [x, y]) == [1]


solve :: Integer -> [(Integer, Integer)]
solve size = do
  (x,y) <- traceUpperEdge 3 4 -- The beam is broken at the beginning!
  guard $ inBeam (x-size+1) (y+size-1)
  pure (x-size+1,y)

traceUpperEdge :: Integer -> Integer -> [(Integer, Integer)]
traceUpperEdge x y
  | inBeam (x+1) y  = traceUpperEdge (x+1) y
  | otherwise = (x,y) : traceUpperEdge x (y+1)


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

program = [109,424,203,1,21101,11,0,0,1105,1,282,21102,1,18,0,1106,0,259,1201,1,0,221,203,1,21101,0,31,0,1106,0,282,21101,0,38,0,1105,1,259,21002,23,1,2,21201,1,0,3,21101,0,1,1,21101,0,57,0,1106,0,303,2102,1,1,222,21002,221,1,3,20102,1,221,2,21102,259,1,1,21101,0,80,0,1105,1,225,21102,1,118,2,21102,91,1,0,1105,1,303,2102,1,1,223,21001,222,0,4,21102,259,1,3,21101,0,225,2,21101,225,0,1,21101,0,118,0,1105,1,225,20101,0,222,3,21102,1,72,2,21102,133,1,0,1106,0,303,21202,1,-1,1,22001,223,1,1,21102,1,148,0,1105,1,259,1201,1,0,223,20101,0,221,4,20101,0,222,3,21101,22,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21102,1,195,0,106,0,108,20207,1,223,2,20101,0,23,1,21102,-1,1,3,21102,214,1,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1201,-4,0,249,22101,0,-3,1,22101,0,-2,2,22101,0,-1,3,21101,0,250,0,1105,1,225,21202,1,1,-4,109,-5,2105,1,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2106,0,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22102,1,-2,-2,109,-3,2105,1,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,22101,0,-2,3,21101,0,343,0,1106,0,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,21202,-4,1,1,21101,384,0,0,1106,0,303,1106,0,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21201,1,0,-4,109,-5,2106,0,0]
