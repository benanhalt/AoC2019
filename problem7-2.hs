{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (forM_)
import Data.List (permutations, sortOn, foldl')

newtype OpCode = OpCode Int
newtype ModeCode = ModeCode Int

data Status = Running | Halted | WaitingForInput | Error
  deriving (Eq, Show)

data State = State
  { pc :: Int
  , mem :: [Int]
  , status :: Status
  , inputs :: [Int]
  , outputs :: [Int]
  } deriving (Show)

program = [3,8,1001,8,10,8,105,1,0,0,21,42,51,60,77,94,175,256,337,418,99999,3,9,1001,9,4,9,102,5,9,9,1001,9,3,9,102,5,9,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,4,9,101,5,9,9,4,9,99,3,9,1002,9,5,9,101,3,9,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99]

main :: IO ()
main = do
  forM_ (sortOn snd possibleThrusts) $ \t ->
    putStrLn $ show t

possibleThrusts :: [([Phase], Int)]
possibleThrusts = do
  phases <- permutations $ Phase <$> [0 .. 4]
  let (Input thrust) = runAmplifierSequence phases
  pure (phases, thrust)

runAmplifierSequence :: [Phase] -> Input
runAmplifierSequence phases =
  foldl' (flip runAmplifier) (Input 0) phases


newtype Phase = Phase Int deriving (Show)
newtype Input = Input Int deriving (Show)

runAmplifier :: Phase -> Input -> Input
runAmplifier (Phase phase) (Input input) = Input $ head outputs
  where
    state0 = State {pc=0, mem=program, status=Running, inputs=[phase, input], outputs=[]}
    State {outputs} = execState state0

execState :: State -> State
execState state0 = stateF
  where
    (running, notRunning) = break (not . isRunning) $ iterate step state0
    stateF = head notRunning

isRunning :: State -> Bool
isRunning (State {status}) = status == Running

step :: State -> State
step s@(State {pc, mem, status}) =
  case status of
    Running ->
      case decodeOp $ mem !! pc of
        (OpCode 1, modes) -> addOp modes s
        (OpCode 2, modes) -> multOp modes s
        (OpCode 3, modes) -> inOp modes s
        (OpCode 4, modes) -> outOp modes s
        (OpCode 5, modes) -> jmpIfTrueOp modes s
        (OpCode 6, modes) -> jmpIfFalseOp modes s
        (OpCode 7, modes) -> ltOp modes s
        (OpCode 8, modes) -> eqOp modes s
        (OpCode 99, modes) -> s {status = Halted}
        _ -> s {status = Error}
    _ -> s

decodeOp :: Int -> (OpCode, [ModeCode])
decodeOp i = (OpCode opCode, ModeCode <$> modeCodes)
  where
    opCode = i `rem` 100
    modeCodes = fmap (`rem` 10) $ iterate (`div` 10) $ i `div` 100

readOperand :: [ModeCode] -> Int -> State -> Int
readOperand modes i (State {pc, mem}) =
  case modes !! i of
    ModeCode 0 -> mem !! val  -- position mode
    ModeCode 1 -> val         -- immediate mode
  where
    val = mem !! (pc + 1 + i)

addOp :: [ModeCode] -> State -> State
addOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    r = mem !! (pc + 3)
    mem' = updateAt r (a + b) mem

multOp :: [ModeCode] -> State -> State
multOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    r = mem !! (pc + 3)
    aTimesb = a * b
    mem' = updateAt r aTimesb mem

inOp :: [ModeCode] -> State -> State
inOp modes s@(State {pc, mem, inputs}) =
  case inputs of
    [] -> s {status = WaitingForInput}
    _ -> s {pc=pc + 2, mem=mem', inputs=tail inputs}
      where
        input = head inputs
        a = mem !! (pc + 1)
        mem' = updateAt a input mem

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
    r = mem !! (pc + 3)
    lt = if a < b then 1 else 0
    mem' = updateAt r lt mem

eqOp :: [ModeCode] -> State -> State
eqOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    r = mem !! (pc + 3)
    eq = if a == b then 1 else 0
    mem' = updateAt r eq mem

updateAt :: Int -> a -> [a] -> [a]
updateAt i a as = h ++ (a : (drop 1 t))
  where (h, t) = splitAt i as
