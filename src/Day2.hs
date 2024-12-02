module Day2
  ( day2,
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Relude

data ReportState = Safe | Unsafe
  deriving (Show, Eq, Ord)

newtype Report = Report [Integer]
  deriving (Show, Eq, Ord)

-- Parsing
parse :: Text -> [Report]
parse input =
  let dataRows = map (Report . map unsafeParseInteger . words) . lines $ input
   in dataRows
  where
    unsafeParseInteger str =
      case TR.decimal str of
        Right (i, _) -> i
        Left _ -> undefined

reportState :: Report -> ReportState
reportState (Report xs) =
  let ds = diffs xs
      s = signs ds
      r = ranges ds
   in if s && r then Safe else Unsafe
  where
    signs xs = all (> 0) xs || all (< 0) xs
    ranges xs = length (filter (\x -> x >= 1 && x <= 3) . map abs $ xs) == length xs

reportState' :: Report -> ReportState
reportState' (Report xs) =
  let xss = variations xs
   in if any ((== Safe) . reportState . Report) xss then Safe else Unsafe
  where
    variations xs =
      let is = [0 .. length xs - 1]
          xss = repeat xs
          pre = zipWith take is xss
          post = map (drop 1) . zipWith drop is $ xss
       in xs : zipWith (++) pre post

diffs :: [Integer] -> [Integer]
diffs [a] = []
diffs (a : b : rest) = (a - b) : diffs (b : rest)

part1 :: Text -> Integer
part1 = toInteger . length . filter (== Safe) . map reportState . parse

part2 :: Text -> Integer
part2 = toInteger . length . filter (== Safe) . map reportState' . parse

day2 :: Text -> IO (String, String)
day2 input = do
  return (show $ part1 input, show $ part2 input)
