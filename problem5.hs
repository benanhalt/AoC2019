{-# LANGUAGE NamedFieldPuns #-}

newtype OpCode = OpCode Int
newtype ModeCode = ModeCode Int

data State = State
  { pc :: Int
  , mem :: [Int]
  , halted :: Bool
  , inputs :: [Int]
  , outputs :: [Int]
  } deriving (Show)


state0 = State
  { halted = False
  , pc = 0
  , mem = [3,225,1,225,6,6,1100,1,238,225,104,0,1002,92,42,224,1001,224,-3444,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1102,24,81,225,1101,89,36,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,118,191,224,101,-880,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,68,94,225,1101,85,91,225,1102,91,82,225,1102,85,77,224,101,-6545,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1101,84,20,225,102,41,36,224,101,-3321,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1,188,88,224,101,-183,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1001,84,43,224,1001,224,-137,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1102,71,92,225,1101,44,50,225,1102,29,47,225,101,7,195,224,101,-36,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1108,226,677,224,102,2,223,223,1006,224,344,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,8,677,226,224,1002,223,2,223,1006,224,374,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,419,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,434,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,464,1001,223,1,223,1007,677,226,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,226,226,224,102,2,223,223,1006,224,494,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,524,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,539,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,554,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,569,101,1,223,223,108,226,226,224,1002,223,2,223,1005,224,584,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,599,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,614,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,659,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226]
  , inputs = [1]
  , outputs = []
  }


main :: IO ()
main = do
  putStrLn $ show $ executeState state0


executeState :: State -> State
executeState state = head notRunning
  where
    (running, notRunning) = break halted $ iterate step state

step :: State -> State
step s@(State {pc, mem, halted}) =
  if halted then s
  else case decodeOp $ mem !! pc of
    (OpCode 1, modes) -> addOp modes s
    (OpCode 2, modes) -> multOp modes s
    (OpCode 3, modes) -> inOp modes s
    (OpCode 4, modes) -> outOp modes s
    (OpCode 99, modes) -> s {halted = True}

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
inOp modes s@(State {pc, mem, inputs}) = s {pc=pc + 2, mem=mem', inputs=tail inputs}
  where
    input = head inputs
    a = mem !! (pc + 1)
    mem' = updateAt a input mem

outOp :: [ModeCode] -> State -> State
outOp modes s@(State {pc, mem, outputs}) = s {pc=pc + 2, outputs=(a : outputs)}
  where
    a = readOperand modes 0 s

updateAt :: Int -> a -> [a] -> [a]
updateAt i a as = h ++ (a : (drop 1 t))
  where (h, t) = splitAt i as
