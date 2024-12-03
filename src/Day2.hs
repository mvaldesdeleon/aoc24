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
parse input = Report . map unsafeParseInteger . words <$> lines input
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
    ranges xs = all ((\x -> x >= 1 && x <= 3) . abs) xs

reportState' :: Report -> ReportState
reportState' (Report xs) =
  let xss = variations xs
   in if any ((== Safe) . reportState . Report) xss then Safe else Unsafe
  where
    variations xs =
      let pre = inits xs
          post = drop 1 <$> tails xs
       in zipWith (++) pre post

diffs :: [Integer] -> [Integer]
diffs xs = zipWith (-) xs (drop 1 xs)

part1 :: Text -> Integer
part1 = toInteger . length . filter (== Safe) . map reportState . parse

part2 :: Text -> Integer
part2 = toInteger . length . filter (== Safe) . map reportState' . parse

day2 :: Text -> IO (String, String)
day2 input = do
  return (show $ part1 input, show $ part2 input)