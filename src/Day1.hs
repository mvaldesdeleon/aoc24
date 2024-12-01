module Day1
  ( day1,
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Relude

-- Parsing
parse :: Text -> ([Integer], [Integer])
parse input =
  let dataRows = map (map (unsafeParseInteger . T.unpack) . words) . lines $ input
      (lhl : rhl : _) = transpose dataRows
   in (lhl, rhl)
  where
    unsafeParseInteger str =
      case readMaybe str of
        Just i -> i
        Nothing -> undefined

part1 :: Text -> Integer
part1 input =
  let (lhl, rhl) = parse input
      slhl = sort lhl
      srhl = sort rhl
   in sum $ map abs $ zipWith (-) slhl srhl

frequencies :: [Integer] -> M.Map Integer Integer
frequencies = foldr countItem M.empty
  where
    countItem i = M.insertWith (+) i 1

part2 :: Text -> Integer
part2 input =
  let (lhl, rhl) = parse input
      freqs = frequencies rhl
      similaritySum i r = r + i * M.findWithDefault 0 i freqs
   in foldr similaritySum 0 lhl

day1 :: Text -> IO (String, String)
day1 input = do
  return (show $ part1 input, show $ part2 input)
