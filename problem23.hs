{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Prelude hiding (Left, Right)
import Control.Monad (forM_, guard)
import Control.Applicative ((<|>))
import Data.List (permutations, sortOn, foldl', nub, unfoldr, splitAt, find, isPrefixOf, iterate')
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Char (chr, ord, toUpper)
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)
import Debug.Trace


main :: IO ()
main =
  let
    states = take 200 $ iterate' stepNetwork network0
  in
    forM_ states (print . nat)


data Network = Network
  { nics :: [State]
  , nat :: Maybe (Integer, Integer)
  }

network0 :: Network
network0 = Network {nics = (\i -> state0 program [i]) <$> [0..49], nat = Nothing}

stepNetwork :: Network -> Network
stepNetwork network@Network{nics, nat} =
  let
    nics' = execState <$> clearOutput <$> nics
    outputs = concat $ (chunk 3 . getOutput) <$> nics'
    nat' = (listToMaybe $ reverse $ (\[_, x, y] -> (x, y)) <$> filter ([255] `isPrefixOf`) outputs)
      <|> nat
  in
    handleNat $ network {nics = sendPackets outputs nics', nat = nat'}

handleNat :: Network -> Network
handleNat network@Network{nics, nat} =
  case nat of
    Just (x, y) | isIdle network -> network {nics = sendPackets [[0, x, y]] nics, nat = Nothing}
    _ -> network

isIdle :: Network -> Bool
isIdle Network{nics} = (all null $ getOutput <$> nics) && (all (== [-1]) $ inputs <$> nics)

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . unfoldr (Just . splitAt n)

sendPackets :: [[Integer]] -> [State] -> [State]
sendPackets outputs nics = do
  (i, nic) <- zip [0..] nics
  let packets = drop 1 <$> filter ([i] `isPrefixOf`) outputs
  let inputs = case concat $ packets of
        [] -> [(-1)]
        ps -> ps
  pure $ provideInputs inputs nic


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

provideInputs :: [Integer] -> State -> State
provideInputs newInputs s@State {inputs, status} = s {inputs=inputs <> newInputs, status=status'}
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

program = [3,62,1001,62,11,10,109,2223,105,1,0,1330,1998,1635,932,903,1297,1600,1435,1064,1664,1967,2192,1730,1400,1501,1200,1827,961,672,635,1371,2027,996,1231,1031,794,1899,2058,1132,732,2120,2089,1862,1468,571,602,703,1934,1792,1532,858,1165,761,1099,2161,1563,1761,829,1260,1699,0,0,0,0,0,0,0,0,0,0,0,0,3,64,1008,64,-1,62,1006,62,88,1006,61,170,1106,0,73,3,65,20101,0,64,1,21002,66,1,2,21101,0,105,0,1106,0,436,1201,1,-1,64,1007,64,0,62,1005,62,73,7,64,67,62,1006,62,73,1002,64,2,133,1,133,68,133,102,1,0,62,1001,133,1,140,8,0,65,63,2,63,62,62,1005,62,73,1002,64,2,161,1,161,68,161,1102,1,1,0,1001,161,1,169,101,0,65,0,1102,1,1,61,1101,0,0,63,7,63,67,62,1006,62,203,1002,63,2,194,1,68,194,194,1006,0,73,1001,63,1,63,1105,1,178,21102,1,210,0,106,0,69,1201,1,0,70,1101,0,0,63,7,63,71,62,1006,62,250,1002,63,2,234,1,72,234,234,4,0,101,1,234,240,4,0,4,70,1001,63,1,63,1106,0,218,1106,0,73,109,4,21102,1,0,-3,21102,0,1,-2,20207,-2,67,-1,1206,-1,293,1202,-2,2,283,101,1,283,283,1,68,283,283,22001,0,-3,-3,21201,-2,1,-2,1105,1,263,22102,1,-3,-3,109,-4,2106,0,0,109,4,21101,1,0,-3,21101,0,0,-2,20207,-2,67,-1,1206,-1,342,1202,-2,2,332,101,1,332,332,1,68,332,332,22002,0,-3,-3,21201,-2,1,-2,1105,1,312,22102,1,-3,-3,109,-4,2106,0,0,109,1,101,1,68,359,20102,1,0,1,101,3,68,366,21002,0,1,2,21101,376,0,0,1106,0,436,21202,1,1,0,109,-1,2105,1,0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,35184372088832,70368744177664,140737488355328,281474976710656,562949953421312,1125899906842624,109,8,21202,-6,10,-5,22207,-7,-5,-5,1205,-5,521,21102,0,1,-4,21102,0,1,-3,21101,0,51,-2,21201,-2,-1,-2,1201,-2,385,470,21001,0,0,-1,21202,-3,2,-3,22207,-7,-1,-5,1205,-5,496,21201,-3,1,-3,22102,-1,-1,-5,22201,-7,-5,-7,22207,-3,-6,-5,1205,-5,515,22102,-1,-6,-5,22201,-3,-5,-3,22201,-1,-4,-4,1205,-2,461,1105,1,547,21102,1,-1,-4,21202,-6,-1,-6,21207,-7,0,-5,1205,-5,547,22201,-7,-6,-7,21201,-4,1,-4,1106,0,529,21201,-4,0,-7,109,-8,2105,1,0,109,1,101,1,68,564,20102,1,0,0,109,-1,2106,0,0,1102,1,78989,66,1101,0,1,67,1102,1,598,68,1101,0,556,69,1101,1,0,71,1102,600,1,72,1105,1,73,1,88867,38,108986,1101,0,90263,66,1102,1,1,67,1101,629,0,68,1101,0,556,69,1101,0,2,71,1102,1,631,72,1106,0,73,1,3,13,49853,32,358724,1101,77551,0,66,1102,1,4,67,1102,1,662,68,1101,0,253,69,1102,1,1,71,1101,0,670,72,1105,1,73,0,0,0,0,0,0,0,0,24,52289,1101,0,41597,66,1102,1,1,67,1101,0,699,68,1102,1,556,69,1101,1,0,71,1102,1,701,72,1105,1,73,1,392570,26,97397,1101,0,15299,66,1101,1,0,67,1102,1,730,68,1102,556,1,69,1102,0,1,71,1102,1,732,72,1106,0,73,1,1700,1102,63977,1,66,1101,0,1,67,1101,759,0,68,1101,556,0,69,1102,0,1,71,1102,761,1,72,1106,0,73,1,1022,1102,1,101273,66,1101,1,0,67,1101,788,0,68,1101,556,0,69,1102,1,2,71,1101,790,0,72,1106,0,73,1,1097,32,179362,17,88169,1101,6961,0,66,1102,3,1,67,1101,0,821,68,1102,1,302,69,1102,1,1,71,1102,1,827,72,1105,1,73,0,0,0,0,0,0,5,201386,1101,52667,0,66,1102,1,1,67,1102,856,1,68,1101,556,0,69,1102,0,1,71,1102,858,1,72,1105,1,73,1,1938,1102,29879,1,66,1102,1,1,67,1101,0,885,68,1102,556,1,69,1101,8,0,71,1102,1,887,72,1106,0,73,1,2,13,99706,32,89681,28,84673,7,104774,43,30722,25,13922,30,301773,30,502955,1101,33287,0,66,1101,0,1,67,1101,0,930,68,1101,0,556,69,1101,0,0,71,1102,1,932,72,1106,0,73,1,1000,1102,1,38377,66,1101,1,0,67,1102,1,959,68,1102,556,1,69,1102,0,1,71,1101,0,961,72,1106,0,73,1,1041,1101,0,88169,66,1101,3,0,67,1101,0,988,68,1101,302,0,69,1102,1,1,71,1102,994,1,72,1105,1,73,0,0,0,0,0,0,41,60982,1101,64151,0,66,1101,3,0,67,1102,1,1023,68,1102,1,302,69,1102,1,1,71,1102,1,1029,72,1106,0,73,0,0,0,0,0,0,8,97381,1102,52289,1,66,1102,1,2,67,1101,1058,0,68,1101,351,0,69,1102,1,1,71,1102,1,1062,72,1106,0,73,0,0,0,0,255,54679,1101,0,97381,66,1102,3,1,67,1102,1091,1,68,1102,253,1,69,1101,0,1,71,1101,0,1097,72,1105,1,73,0,0,0,0,0,0,33,79283,1102,1,15361,66,1101,0,2,67,1102,1126,1,68,1102,302,1,69,1101,0,1,71,1101,1130,0,72,1106,0,73,0,0,0,0,25,6961,1102,1,84673,66,1102,1,2,67,1102,1,1159,68,1102,1,302,69,1101,0,1,71,1102,1163,1,72,1106,0,73,0,0,0,0,7,52387,1102,30491,1,66,1101,3,0,67,1101,1192,0,68,1102,302,1,69,1102,1,1,71,1102,1198,1,72,1105,1,73,0,0,0,0,0,0,19,77551,1102,1,71059,66,1102,1,1,67,1101,1227,0,68,1102,1,556,69,1102,1,1,71,1101,1229,0,72,1106,0,73,1,6827,22,64151,1101,14783,0,66,1101,1,0,67,1102,1258,1,68,1102,556,1,69,1102,0,1,71,1101,1260,0,72,1105,1,73,1,1547,1102,1,30977,66,1102,4,1,67,1102,1,1287,68,1102,302,1,69,1102,1,1,71,1101,0,1295,72,1105,1,73,0,0,0,0,0,0,0,0,30,201182,1102,1,100693,66,1101,2,0,67,1102,1,1324,68,1102,302,1,69,1102,1,1,71,1101,1328,0,72,1106,0,73,0,0,0,0,19,232653,1101,54679,0,66,1101,1,0,67,1101,0,1357,68,1102,1,556,69,1102,6,1,71,1102,1,1359,72,1105,1,73,1,22130,5,100693,41,30491,41,91473,16,1151,16,2302,16,3453,1102,41851,1,66,1102,1,1,67,1101,1398,0,68,1101,556,0,69,1101,0,0,71,1101,0,1400,72,1106,0,73,1,1407,1101,0,49853,66,1101,0,3,67,1102,1,1427,68,1101,0,302,69,1102,1,1,71,1101,1433,0,72,1106,0,73,0,0,0,0,0,0,32,269043,1102,52387,1,66,1101,0,2,67,1102,1462,1,68,1102,302,1,69,1101,1,0,71,1102,1466,1,72,1106,0,73,0,0,0,0,43,15361,1101,0,79283,66,1101,2,0,67,1102,1,1495,68,1101,0,302,69,1101,0,1,71,1101,0,1499,72,1105,1,73,0,0,0,0,13,149559,1102,1,20759,66,1101,1,0,67,1102,1528,1,68,1101,0,556,69,1101,0,1,71,1101,0,1530,72,1106,0,73,1,191,25,20883,1101,75557,0,66,1102,1,1,67,1102,1559,1,68,1102,1,556,69,1102,1,1,71,1102,1561,1,72,1105,1,73,1,32,38,54493,1101,0,10589,66,1102,1,1,67,1102,1590,1,68,1101,0,556,69,1102,4,1,71,1101,0,1592,72,1106,0,73,1,1,22,128302,9,136978,38,163479,17,176338,1102,22877,1,66,1101,1,0,67,1101,0,1627,68,1102,556,1,69,1101,3,0,71,1102,1,1629,72,1105,1,73,1,5,48,92931,48,123908,30,100591,1102,5683,1,66,1101,0,1,67,1102,1,1662,68,1101,0,556,69,1102,0,1,71,1102,1,1664,72,1106,0,73,1,1699,1101,68489,0,66,1102,1,3,67,1102,1,1691,68,1102,302,1,69,1101,1,0,71,1102,1,1697,72,1106,0,73,0,0,0,0,0,0,8,292143,1101,0,83257,66,1102,1,1,67,1102,1726,1,68,1102,556,1,69,1102,1,1,71,1102,1,1728,72,1106,0,73,1,138,9,205467,1102,50363,1,66,1102,1,1,67,1102,1,1757,68,1102,556,1,69,1102,1,1,71,1101,0,1759,72,1105,1,73,1,16,33,158566,1102,91193,1,66,1101,1,0,67,1101,0,1788,68,1102,1,556,69,1102,1,1,71,1102,1,1790,72,1105,1,73,1,-119,22,192453,1102,54493,1,66,1101,0,3,67,1102,1,1819,68,1102,302,1,69,1101,1,0,71,1102,1,1825,72,1106,0,73,0,0,0,0,0,0,8,194762,1102,1151,1,66,1101,0,3,67,1102,1,1854,68,1102,302,1,69,1102,1,1,71,1101,0,1860,72,1105,1,73,0,0,0,0,0,0,19,155102,1102,89681,1,66,1101,4,0,67,1101,0,1889,68,1102,1,302,69,1101,0,1,71,1102,1897,1,72,1106,0,73,0,0,0,0,0,0,0,0,19,310204,1102,1,97397,66,1102,3,1,67,1102,1,1926,68,1101,253,0,69,1101,0,1,71,1102,1,1932,72,1105,1,73,0,0,0,0,0,0,28,169346,1101,47297,0,66,1101,0,1,67,1102,1961,1,68,1102,556,1,69,1102,1,2,71,1101,0,1963,72,1106,0,73,1,10,48,30977,30,402364,1102,1,100799,66,1102,1,1,67,1102,1994,1,68,1102,556,1,69,1102,1,1,71,1102,1,1996,72,1106,0,73,1,125,48,61954,1101,42953,0,66,1101,1,0,67,1102,1,2025,68,1101,0,556,69,1101,0,0,71,1101,2027,0,72,1105,1,73,1,1507,1101,0,95789,66,1102,1,1,67,1102,2054,1,68,1102,1,556,69,1101,1,0,71,1102,1,2056,72,1106,0,73,1,-28047,26,194794,1101,0,82051,66,1102,1,1,67,1101,0,2085,68,1102,1,556,69,1102,1,1,71,1102,1,2087,72,1106,0,73,1,4409,9,68489,1102,1,35069,66,1102,1,1,67,1102,1,2116,68,1101,556,0,69,1102,1,1,71,1102,2118,1,72,1106,0,73,1,-36,17,264507,1102,100591,1,66,1101,6,0,67,1101,0,2147,68,1101,0,302,69,1102,1,1,71,1101,0,2159,72,1106,0,73,0,0,0,0,0,0,0,0,0,0,0,0,24,104578,1102,73459,1,66,1102,1,1,67,1102,1,2188,68,1102,556,1,69,1101,0,1,71,1101,0,2190,72,1106,0,73,1,34650,26,292191,1101,0,81971,66,1102,1,1,67,1101,0,2219,68,1101,0,556,69,1102,1,1,71,1102,1,2221,72,1106,0,73,1,160,30,603546]
