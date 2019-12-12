{-# LANGUAGE NamedFieldPuns #-}

import Data.List (iterate')

data Moon = Moon
  { position :: [Int]
  , velocity :: [Int]
  } deriving (Show, Eq)


gravity :: Moon -> Moon -> Moon
gravity Moon{position = a} affected@Moon{position = b, velocity} =
  affected {velocity = zipWith (+) velocity (signum <$> zipWith (-) a b)}

allGravity :: [Moon] -> [Moon]
allGravity moons = flip (foldr gravity) moons <$> moons

updatePosition :: Moon -> Moon
updatePosition moon@Moon{position, velocity} = moon {position = zipWith (+) velocity position}

step :: [Moon] -> [Moon]
step moons = updatePosition <$> allGravity moons

initMoon :: (Int, Int, Int) -> Moon
initMoon (x, y, z) = Moon {position = [x,y,z], velocity=[0,0,0]}

energy :: Moon -> Int
energy Moon{position, velocity} = ke * pe
  where
   ke = sum $ abs <$> position
   pe = sum $ abs <$> velocity

givenMoons :: [Moon]
givenMoons = initMoon <$>
  [ (1, 4, 4)
  , (-4, -1, 19)
  , (-15, -14, 12)
  , (-17, 1, 10)
  ]

exampleMoons :: [Moon]
exampleMoons = initMoon <$>
  [ (-1, 0, 2)
  , (2, -10, -7)
  , (4, -8, 8)
  , (3, 5, -1)
  ]

simulate :: [Moon] -> [Moon] -> Int -> Int
simulate init !moons !count =
  if moons == init
  then count
  else simulate init (step moons) (count + 1)

main :: IO ()
main = do
  print $ sum $ energy <$> (iterate step givenMoons !! 10)
  print $ simulate givenMoons (step givenMoons) 1

