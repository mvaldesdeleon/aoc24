module Day10
  ( day10,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data IslandMap = IslandMap
  { imWidth :: Integer,
    imHeight :: Integer,
    imMap :: V.Vector Int
  }
  deriving (Eq, Ord, Show)

parseIslandMap :: Text -> IslandMap
parseIslandMap input =
  let rows = lines input
      height = length rows
      width = maybe 0 T.length (viaNonEmpty head rows)
   in IslandMap (toInteger width) (toInteger height) (V.fromList . map toInt . T.unpack . T.filter (/= '\n') $ input)
  where
    toInt c = fromEnum c - fromEnum '0'

trailheads :: IslandMap -> [Integer]
trailheads = map toInteger . V.toList . V.findIndices (== 0) . imMap

trailheadTrails :: IslandMap -> Integer -> [Integer]
trailheadTrails (IslandMap width heigth map) th = go (toCoords th) (-1) []
  where
    toCoords p = (p `mod` width, p `div` width)
    go (x, y) prev score =
      if x >= 0 && x < width && y >= 0 && y < heigth
        then
          let th = y * width + x
              next = map V.! fromInteger th
           in if next - prev == 1
                then
                  if next == 9
                    then th : score
                    else score ++ go (x + 1, y) next score ++ go (x, y + 1) next score ++ go (x - 1, y) next score ++ go (x, y - 1) next score
                else score
        else score

trailheadScore :: IslandMap -> Integer -> Integer
trailheadScore im th = L.genericLength . L.nub $ trailheadTrails im th

trailheadRating :: IslandMap -> Integer -> Integer
trailheadRating im th = L.genericLength $ trailheadTrails im th

part1 :: Text -> Integer
part1 input =
  let im = parseIslandMap input
      ths = trailheads im
   in sum $ trailheadScore im <$> ths

part2 :: Text -> Integer
part2 input =
  let im = parseIslandMap input
      ths = trailheads im
   in sum $ trailheadRating im <$> ths

day10 :: Text -> IO (String, String)
day10 input = do
  return (show $ part1 input, show $ part2 input)
