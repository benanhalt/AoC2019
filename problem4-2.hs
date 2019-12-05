

import Control.Monad (guard)
import Data.List (group, any, all)


hasDouble :: String -> Bool
hasDouble xs = any (== 2) $ fmap length $ group xs

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
