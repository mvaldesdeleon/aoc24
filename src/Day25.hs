module Day25
  ( day25,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Relude

data Schematic = Lock [Int] | Key [Int]
  deriving (Eq, Ord, Show)

isLock :: Schematic -> Bool
isLock (Lock _) = True
isLock _ = False

parseSchematics :: Text -> ([Schematic], [Schematic])
parseSchematics input =
  let schematics = parseSchematic <$> T.splitOn "\n\n" input
   in L.span isLock (L.sort schematics)
  where
    parseSchematic input =
      let rows = lines input
          cols = T.transpose rows
       in if L.head rows == "#####"
            then Lock $ pinSize <$> cols
            else Key $ pinSize . T.reverse <$> cols
    pinSize = pred . T.length . T.takeWhile (== '#')

overlap :: (Schematic, Schematic) -> Bool
overlap (Lock lc, Key kc) = any (> 5) $ zipWith (+) lc kc

part1 :: Text -> Integer
part1 input =
  let (locks, keys) = parseSchematics input
      pairs = (,) <$> locks <*> keys
      overlaps = sum $ fromEnum . overlap <$> pairs
   in toInteger $ length pairs - overlaps

day25 :: Text -> IO (String, String)
day25 input = do
  return (show $ part1 input, "N/A")
