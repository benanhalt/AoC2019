{-# LANGUAGE NamedFieldPuns #-}


data State = State { pc :: Int, mem :: [Int], halted :: Bool } deriving (Show)

-- state0 = State
--   { halted = False
--   , pc = 0
--   , mem = [1,9,10,3,2,3,11,0,99,30,40,50]
--   }

state0 = State
  { halted = False
  , pc = 0
  , mem = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,2,6,23,27,1,6,27,31,2,31,9,35,1,35,6,39,1,10,39,43,2,9,43,47,1,5,47,51,2,51,6,55,1,5,55,59,2,13,59,63,1,63,5,67,2,67,13,71,1,71,9,75,1,75,6,79,2,79,6,83,1,83,5,87,2,87,9,91,2,9,91,95,1,5,95,99,2,99,13,103,1,103,5,107,1,2,107,111,1,111,5,0,99,2,14,0,0]
  }


main :: IO ()
main = do
  let run = iterate step state0
  let (running, notRunning) = break halted run
  putStrLn $ show running
  putStrLn $ show $ head notRunning

step :: State -> State
step s@(State {pc, mem, halted}) =
  if halted then s
  else case mem !! pc of
    1 -> addOp s
    2 -> multOp s
    99 -> s {halted = True}


addOp :: State -> State
addOp s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = mem !! (pc + 1)
    b = mem !! (pc + 2)
    r = mem !! (pc + 3)
    aPlusb = (mem !! a) + (mem !! b)
    mem' = updateAt r aPlusb mem

multOp :: State -> State
multOp s@(State {pc, mem}) = s {pc=pc + 4, mem=mem'}
  where
    a = mem !! (pc + 1)
    b = mem !! (pc + 2)
    r = mem !! (pc + 3)
    aTimesb = (mem !! a) * (mem !! b)
    mem' = updateAt r aTimesb mem

updateAt :: Int -> a -> [a] -> [a]
updateAt i a as = h ++ (a : (drop 1 t))
  where (h, t) = splitAt i as
