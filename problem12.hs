{-# LANGUAGE NamedFieldPuns, DeriveGeneric #-}

import Data.List (iterate')
import Control.DeepSeq (NFData, deepseq)
import GHC.Generics (Generic)

data Moon = Moon
  { position :: [Int]
  , velocity :: [Int]
  } deriving (Show, Eq, Generic)

instance NFData Moon

gravity :: Moon -> Moon -> Moon
gravity Moon{position = a} affected@Moon{position = b, velocity} =
  affected {velocity = zipWith (\a b -> a + b) velocity (signum <$> zipWith (-) a b)}

allGravity :: [Moon] -> [Moon]
allGravity moons = flip (foldr gravity) moons <$> moons

updatePosition :: Moon -> Moon
updatePosition moon@Moon{position, velocity} =
  moon `deepseq` moon {position = zipWith (+) position velocity}

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

simulate :: [[Moon]] -> [[Moon]]
simulate (moons:past) =
  if elem moons past
  then past
  else simulate (step moons : moons : past)

main :: IO ()
main = do
  print $ sum $ energy <$> (iterate step givenMoons !! 1000)
  print $ length $ simulate [exampleMoons]
