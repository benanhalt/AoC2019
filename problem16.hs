import Control.DeepSeq
import Debug.Trace
import Data.List (iterate')
import qualified Data.Vector.Unboxed as V


input = "59708372326282850478374632294363143285591907230244898069506559289353324363446827480040836943068215774680673708005813752468017892971245448103168634442773462686566173338029941559688604621181240586891859988614902179556407022792661948523370366667688937217081165148397649462617248164167011250975576380324668693910824497627133242485090976104918375531998433324622853428842410855024093891994449937031688743195134239353469076295752542683739823044981442437538627404276327027998857400463920633633578266795454389967583600019852126383407785643022367809199144154166725123539386550399024919155708875622641704428963905767166129198009532884347151391845112189952083025"

main :: IO ()
main = do
  print $ take 8 $ fft 100 input
--   print $ part2 input


part2 :: String -> String
part2 input = take 8 $ fft 1 input'
  --take 8 $ drop offset $ fft 100 input'
  where
    -- offset = read $ take 7 input
    input' = concat $ take 10000 $ repeat input

fft :: Int -> String -> String
fft n s = concat $ show <$> V.toList (iterate' phase input !! n)
  where
    input = V.fromList $ (read . pure) <$> s

phase :: V.Vector Int -> V.Vector Int
phase input = V.generate (V.length input) (phaseElement input)

phaseElement :: V.Vector Int -> Int -> Int
phaseElement input pos = (`mod` 10) $ abs $ V.ifoldl' (\a i inp -> a + inp * base !! ((i+1) `div` (pos+1) `mod` 4)) 0 input
  where
    base = [0, 1, 0, -1]

-- pattern' :: Int -> Int -> V.Vector Int
-- pattern' l n = V.generate l generator
--   where
--     generator i = base !! ((i+1) `div` n `mod` 4)
--     base = [0, 1, 0, -1]
