module Day13
  ( day13,
  )
where

import Data.Attoparsec.Text hiding (parse)
import qualified Data.List as L
import Data.NumInstances.Tuple
import Data.Ratio
import Relude

data ClawMachine = ClawMachine
  { cmButtonA :: (Integer, Integer),
    cmButtonB :: (Integer, Integer),
    cmPrize :: (Integer, Integer)
  }
  deriving (Eq, Ord, Show)

parseButton :: Parser (Integer, Integer)
parseButton = (,) <$> ("Button " *> anyChar *> ": X+" *> decimal) <*> (", Y+" *> decimal)

parsePrize :: Parser (Integer, Integer)
parsePrize = (,) <$> ("Prize: X=" *> decimal) <*> (", Y=" *> decimal)

parseClawMachine :: Parser ClawMachine
parseClawMachine = ClawMachine <$> parseButton <* endOfLine <*> parseButton <* endOfLine <*> parsePrize

tokens :: (Num a) => (a, a) -> a
tokens (a, b) = 3 * a + b

fixPrize :: ClawMachine -> ClawMachine
fixPrize cm@ClawMachine {cmPrize = p} = cm {cmPrize = p + (10000000000000, 10000000000000)}

solve :: ClawMachine -> Maybe [(Integer, Integer)]
solve (ClawMachine btnA btnB prize) =
  let slns = filter isSolution [(a, b) | a <- [0 .. 100], b <- [0 .. 100]]
   in if null slns then Nothing else Just slns
  where
    isSolution :: (Integer, Integer) -> Bool
    isSolution (a, b) = fromInteger a * btnA + fromInteger b * btnB == prize

solveCheapest :: ClawMachine -> Maybe (Integer, Integer)
solveCheapest cm = findCheapest <$> solve cm
  where
    findCheapest = L.minimumBy (compare `on` tokens)

parse :: Text -> [ClawMachine]
parse input = case parseOnly (parseClawMachine `sepBy1` count 2 endOfLine) input of
  Left err -> error $ toText err
  Right cms -> cms

part1 :: Text -> Integer
part1 input =
  let cms = parse input
   in sum $ tokens <$> mapMaybe solveCheapest cms

solve' :: ClawMachine -> Maybe (Integer, Integer)
solve' (ClawMachine (xa, ya) (xb, yb) (xp, yp)) =
  let n = ya % xa
      b = (yp % 1 - xp % 1 * n) / (yb % 1 - xb % 1 * n)
      a = (yp % 1 - yb % 1 * b) / (ya % 1)
   in if denominator a == 1 && denominator b == 1
        then
          Just (numerator a, numerator b)
        else
          Nothing

part2 :: Text -> Integer
part2 input =
  let cms = fixPrize <$> parse input
   in sum $ tokens <$> mapMaybe solve' cms

day13 :: Text -> IO (String, String)
day13 input = do
  return (show $ part1 input, show $ part2 input)
