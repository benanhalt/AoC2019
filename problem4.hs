

import Control.Monad (guard)


hasDouble :: String -> Bool
hasDouble "" = False
hasDouble [_] = False
hasDouble (x : y : r) | x == y = True
                      | otherwise = hasDouble (y : r)

increasing :: String -> Bool
increasing "" = True
increasing [_] = True
increasing (x : y : r) | x > y = False
                       | otherwise = increasing (y : r)

candidates :: Int -> Int -> [Int]
candidates start end = do
  x <- [start .. end]
  let x' = show x
  guard $ (hasDouble x') && (increasing x')
  pure x

main :: IO ()
main = putStrLn $ show $ length $ candidates 153517 630395
