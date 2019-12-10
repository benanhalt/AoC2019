{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (forM_)
import Data.List (permutations, sortOn, foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

(!!!) :: Map.Map Integer Integer -> Integer -> Integer
mem !!! i = fromMaybe 0 $ Map.lookup i mem

newtype OpCode = OpCode Integer
newtype ModeCode = ModeCode Integer

data Status = Running | Halted | WaitingForInput | Error
  deriving (Eq, Show)

data State = State
  { pc :: Integer
  , mem :: Map.Map Integer Integer
  , status :: Status
  , inputs :: [Integer]
  , outputs :: [Integer]
  } deriving (Show)

program = [3,8,1001,8,10,8,105,1,0,0,21,42,51,60,77,94,175,256,337,418,99999,3,9,1001,9,4,9,102,5,9,9,1001,9,3,9,102,5,9,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,4,9,101,5,9,9,4,9,99,3,9,1002,9,5,9,101,3,9,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99]

main :: IO ()
main = do
  forM_ (sortOn snd possibleThrusts) $ \t ->
    putStrLn $ show t

possibleThrusts :: [([Phase], Integer)]
possibleThrusts = do
  phases <- permutations $ Phase <$> [5 .. 9]
  let (Input thrust, _) = execSequence phases
  pure (phases, thrust)


execSequence :: [Phase] -> SequenceState
execSequence phases = head finished
  where
    sequenceState0 = (Input 0, startAmplifier <$> phases)
    (running, finished) = break isFinished $ iterate runSequence sequenceState0

isFinished :: SequenceState -> Bool
isFinished (_, states) = (status $ last states) == Halted

type SequenceState = (Input, [State])

runSequence :: SequenceState -> SequenceState
runSequence (input, states) = (output, states')
  where
    states' = runAmplifierSequence input states []
    output = getOutput $ last states'


runAmplifierSequence :: Input -> [State] -> [State] -> [State]
runAmplifierSequence input toRun finished =
  case toRun of
    [] -> finished
    (state : toRun') -> runAmplifierSequence output toRun' (finished <> [state'])
      where
        state' = runAmplifier state input
        output = getOutput state'

getOutput :: State -> Input
getOutput (State {outputs}) = Input $ head outputs

newtype Phase = Phase Integer deriving (Show)
newtype Input = Input Integer deriving (Show)

startAmplifier :: Phase -> State
startAmplifier (Phase phase) =
  execState $ State {pc=0, mem=Map.fromList $ zip [0..] program, status=Running, inputs=[phase], outputs=[]}

runAmplifier :: State -> Input -> State
runAmplifier state0 (Input input) = execState $ provideInput input $ state0

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
        (OpCode 99, modes) -> s {status = Halted}
        _ -> s {status = Error}
    _ -> s

decodeOp :: Integer -> (OpCode, [ModeCode])
decodeOp i = (OpCode opCode, ModeCode <$> modeCodes)
  where
    opCode = i `rem` 100
    modeCodes = fmap (`rem` 10) $ iterate (`div` 10) $ i `div` 100

readOperand :: [ModeCode] -> Int -> State -> Integer
readOperand modes i (State {pc, mem}) =
  case modes !! i of
    ModeCode 0 -> mem !!! val  -- position mode
    ModeCode 1 -> val         -- immediate mode
  where
    val = mem !!! (pc + 1 + toInteger i)

addOp :: [ModeCode] -> State -> State
addOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    r = mem !!! (pc + 3)
    mem' = updateAt r (a + b) mem

multOp :: [ModeCode] -> State -> State
multOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    r = mem !!! (pc + 3)
    aTimesb = a * b
    mem' = updateAt r aTimesb mem

inOp :: [ModeCode] -> State -> State
inOp modes s@(State {pc, mem, inputs}) =
  case inputs of
    [] -> s {status = WaitingForInput}
    _ -> s {pc=pc + 2, mem=mem', inputs=tail inputs}
      where
        input = head inputs
        a = mem !!! (pc + 1)
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
    r = mem !!! (pc + 3)
    lt = if a < b then 1 else 0
    mem' = updateAt r lt mem

eqOp :: [ModeCode] -> State -> State
eqOp modes s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = readOperand modes 0 s
    b = readOperand modes 1 s
    r = mem !!! (pc + 3)
    eq = if a == b then 1 else 0
    mem' = updateAt r eq mem

updateAt :: Integer -> a -> Map.Map Integer a -> Map.Map Integer a
updateAt = Map.insert

