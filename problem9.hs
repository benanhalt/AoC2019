{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (forM_)
import Data.List (permutations, sortOn, foldl')
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

program = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,1,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,0,0,1020,1101,34,0,1004,1101,0,26,1008,1102,1,37,1011,1101,39,0,1018,1102,587,1,1022,1101,1,0,1021,1102,22,1,1012,1101,0,33,1014,1101,24,0,1016,1101,0,752,1029,1101,36,0,1002,1101,35,0,1006,1101,32,0,1009,1102,38,1,1003,1102,584,1,1023,1101,0,20,1001,1102,892,1,1025,1102,29,1,1000,1101,411,0,1026,1102,1,901,1024,1101,0,761,1028,1101,23,0,1017,1102,30,1,1013,1101,0,27,1015,1102,28,1,1005,1101,408,0,1027,1101,25,0,1007,1102,31,1,1019,1101,0,21,1010,109,5,1207,-2,39,63,1005,63,199,4,187,1105,1,203,1001,64,1,64,1002,64,2,64,109,12,21102,40,1,-1,1008,1016,40,63,1005,63,229,4,209,1001,64,1,64,1106,0,229,1002,64,2,64,109,-5,1207,-5,24,63,1005,63,249,1001,64,1,64,1106,0,251,4,235,1002,64,2,64,109,-14,2102,1,6,63,1008,63,32,63,1005,63,271,1106,0,277,4,257,1001,64,1,64,1002,64,2,64,109,2,1202,1,1,63,1008,63,20,63,1005,63,303,4,283,1001,64,1,64,1106,0,303,1002,64,2,64,109,7,2108,34,2,63,1005,63,319,1106,0,325,4,309,1001,64,1,64,1002,64,2,64,109,6,2101,0,-6,63,1008,63,24,63,1005,63,349,1001,64,1,64,1105,1,351,4,331,1002,64,2,64,109,4,21107,41,42,0,1005,1017,369,4,357,1105,1,373,1001,64,1,64,1002,64,2,64,109,5,21101,42,0,-5,1008,1017,41,63,1005,63,397,1001,64,1,64,1106,0,399,4,379,1002,64,2,64,109,9,2106,0,-4,1106,0,417,4,405,1001,64,1,64,1002,64,2,64,109,-20,21108,43,43,0,1005,1011,435,4,423,1105,1,439,1001,64,1,64,1002,64,2,64,109,-15,2102,1,8,63,1008,63,34,63,1005,63,465,4,445,1001,64,1,64,1105,1,465,1002,64,2,64,109,3,1201,6,0,63,1008,63,28,63,1005,63,491,4,471,1001,64,1,64,1106,0,491,1002,64,2,64,109,18,21108,44,46,0,1005,1017,511,1001,64,1,64,1106,0,513,4,497,1002,64,2,64,109,12,1205,-8,527,4,519,1105,1,531,1001,64,1,64,1002,64,2,64,109,-17,1208,-3,32,63,1005,63,553,4,537,1001,64,1,64,1105,1,553,1002,64,2,64,109,-13,1208,10,31,63,1005,63,573,1001,64,1,64,1105,1,575,4,559,1002,64,2,64,109,17,2105,1,7,1105,1,593,4,581,1001,64,1,64,1002,64,2,64,109,-8,2107,19,-7,63,1005,63,615,4,599,1001,64,1,64,1105,1,615,1002,64,2,64,109,4,1206,8,629,4,621,1106,0,633,1001,64,1,64,1002,64,2,64,109,-2,2101,0,-6,63,1008,63,34,63,1005,63,655,4,639,1105,1,659,1001,64,1,64,1002,64,2,64,109,10,1205,0,671,1105,1,677,4,665,1001,64,1,64,1002,64,2,64,109,-21,2107,26,8,63,1005,63,693,1106,0,699,4,683,1001,64,1,64,1002,64,2,64,109,19,1201,-9,0,63,1008,63,30,63,1005,63,719,1105,1,725,4,705,1001,64,1,64,1002,64,2,64,109,9,1206,-6,741,1001,64,1,64,1106,0,743,4,731,1002,64,2,64,109,-5,2106,0,6,4,749,1001,64,1,64,1105,1,761,1002,64,2,64,109,-14,1202,-1,1,63,1008,63,27,63,1005,63,781,1105,1,787,4,767,1001,64,1,64,1002,64,2,64,109,1,21107,45,44,5,1005,1014,807,1001,64,1,64,1105,1,809,4,793,1002,64,2,64,109,8,21101,46,0,0,1008,1017,46,63,1005,63,835,4,815,1001,64,1,64,1106,0,835,1002,64,2,64,109,-26,2108,20,10,63,1005,63,857,4,841,1001,64,1,64,1106,0,857,1002,64,2,64,109,24,21102,47,1,-5,1008,1010,46,63,1005,63,881,1001,64,1,64,1106,0,883,4,863,1002,64,2,64,109,6,2105,1,3,4,889,1001,64,1,64,1105,1,901,4,64,99,21102,27,1,1,21101,915,0,0,1105,1,922,21201,1,29830,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,0,942,0,1105,1,922,21202,1,1,-1,21201,-2,-3,1,21102,1,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2106,0,0]

main :: IO ()
main = do
  print $ getOutput $ execState $ state0 program [1]
  print $ getOutput $ execState $ state0 program [2]

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
    (running, notRunning) = break (not . isRunning) $ iterate step state0
    stateF = head notRunning

isRunning :: State -> Bool
isRunning (State {status}) = status == Running

provideInput :: Integer -> State -> State
provideInput i s@(State {inputs, status}) = s {inputs=inputs <> [i], status=status'}
  where
    status' = case status of
      WaitingForInput -> Running
      _ -> status

getOutput :: State -> [Integer]
getOutput (State {outputs}) = reverse outputs


step :: State -> State
step s@(State {pc, mem, status}) =
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
readOperand modes i (State {pc, mem, relBase}) =
  case modes !! i of
    ModeCode 0 -> mem !!! val  -- position mode
    ModeCode 1 -> val          -- immediate mode
    ModeCode 2 -> mem !!! (val + relBase) -- relative mode
  where
    val = mem !!! (pc + 1 + toInteger i)

writeOperand :: [ModeCode] -> Int -> Integer -> State -> Mem
writeOperand modes i val s@(State {pc, mem, relBase}) = Map.insert k val mem
  where
    j = mem !!! (pc + 1 + toInteger i)
    k = case modes !! i of
      ModeCode 0 -> j           -- position mode
      ModeCode 2 -> j + relBase -- relative mode

addOp :: [ModeCode] -> State -> State
addOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    mem' = writeOperand modes 2 (a + b) s

multOp :: [ModeCode] -> State -> State
multOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    mem' = writeOperand modes 2 (a * b) s

inOp :: [ModeCode] -> State -> State
inOp modes s@(State {pc, mem, inputs}) =
  case inputs of
    [] -> s {status = WaitingForInput}
    _ -> s {pc=pc + 2, mem=mem', inputs=tail inputs}
      where
        input = head inputs
        mem' = writeOperand modes 0 input s

outOp :: [ModeCode] -> State -> State
outOp modes s@(State {pc, mem, outputs}) = s {pc=pc + 2, outputs=(a : outputs)}
  where
    a = readOperand modes 0 s

jmpIfTrueOp :: [ModeCode] -> State -> State
jmpIfTrueOp modes s@(State {pc, mem}) = s {pc=pc'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    pc' = if a == 0 then pc + 3 else b

jmpIfFalseOp :: [ModeCode] -> State -> State
jmpIfFalseOp modes s@(State {pc, mem}) = s {pc=pc'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    pc' = if a == 0 then b else pc + 3

ltOp :: [ModeCode] -> State -> State
ltOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    lt = if a < b then 1 else 0
    mem' = writeOperand modes 2 lt s

eqOp :: [ModeCode] -> State -> State
eqOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    eq = if a == b then 1 else 0
    mem' = writeOperand modes 2 eq s

adjRelBaseOp :: [ModeCode] -> State -> State
adjRelBaseOp modes s@(State {pc, relBase}) = s {pc=pc + 2, relBase=relBase + a}
  where
    a = readOperand modes 0 s
