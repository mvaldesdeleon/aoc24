module Day11
  ( day11,
  )
where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Relude

type Stone = Integer

unsafeParseInteger :: (Integral a) => Text -> a
unsafeParseInteger str =
  case TR.decimal str of
    Right (i, _) -> i
    Left _ -> undefined

parse :: Text -> [Stone]
parse = map unsafeParseInteger . words

rule :: Stone -> [Stone]
rule 0 = [1]
rule s =
  let str = show s :: Text
      ds = T.length str
      hds = ds `div` 2
   in if even ds then unsafeParseInteger <$> [T.take hds str, T.drop hds str] else [s * 2024]

blink :: [Stone] -> [Stone]
blink = concatMap rule

part1 :: Text -> Integer
part1 input =
  let stones = parse input
   in L.genericLength $ L.iterate' blink stones L.!! 25

buildMap :: [Stone] -> M.Map Stone Integer
buildMap = foldr countStones M.empty
  where
    countStones s = M.insertWith (+) s 1

blink' :: M.Map Stone Integer -> M.Map Stone Integer
blink' = M.foldrWithKey' rule' M.empty
  where
    rule' s count map = foldr (countStones count) map (rule s)
    countStones count s = M.insertWith (+) s count

part2 :: Text -> Integer
part2 input =
  let stones = buildMap $ parse input
   in M.foldr' (+) 0 $ L.iterate' blink' stones L.!! 75

day11 :: Text -> IO (String, String)
day11 input = do
  return (show $ part1 input, show $ part2 input)
