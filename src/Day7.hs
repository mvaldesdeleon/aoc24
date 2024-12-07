module Day7
  ( day7,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Numeric (log)
import Relude

data Equation = Equation
  { eqTestValue :: Integer,
    eqNumbers :: [Integer]
  }
  deriving (Eq, Ord, Show)

unsafeParseInteger :: (Integral a) => Text -> a
unsafeParseInteger str =
  case TR.decimal str of
    Right (i, _) -> i
    Left _ -> undefined

parseEquation :: Text -> Equation
parseEquation input =
  let raw = unsafeParseInteger <$> words input
      (tv : nums) = raw
   in Equation tv nums

parse :: Text -> [Equation]
parse input =
  let eqs = T.filter (/= ':') <$> lines input
   in parseEquation <$> eqs

canBeTrue :: Equation -> Bool
canBeTrue (Equation tv nums) = go tv 0 nums
  where
    go tv cv [] = tv == cv
    go tv cv (n : ns) =
      if cv <= tv
        then
          go tv (cv + n) ns || go tv (cv * n) ns
        else
          False

cc :: Integer -> Integer -> Integer
cc a b =
  let c = show a ++ show b
   in unsafeParseInteger (T.pack c)

canBeTrue' :: Equation -> Bool
canBeTrue' (Equation tv nums) = go tv 0 nums
  where
    go tv cv [] = tv == cv
    go tv cv (n : ns) =
      if cv <= tv
        then
          go tv (cv + n) ns || go tv (cv * n) ns || go tv (cv `cc` n) ns
        else
          False

part1 :: Text -> Integer
part1 input =
  let eqs = parse input
   in sum $ eqTestValue <$> filter canBeTrue eqs

part2 :: Text -> Integer
part2 input =
  let eqs = parse input
   in sum $ eqTestValue <$> filter canBeTrue' eqs

day7 :: Text -> IO (String, String)
day7 input = do
  return (show $ part1 input, show $ part2 input)
