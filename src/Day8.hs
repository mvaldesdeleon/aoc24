module Day8
  ( day8,
  )
where

import Data.List (iterate, nub, takeWhile)
import qualified Data.Map.Strict as M
import Data.NumInstances.Tuple
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data Location = Empty | Tower Char
  deriving (Eq, Ord, Show)

toLocation :: Char -> Location
toLocation '.' = Empty
toLocation c = Tower c

data CityMap = CityMap
  { mWidth :: Integer,
    mHeight :: Integer,
    mLocations :: V.Vector Location
  }
  deriving (Ord, Eq, Show)

groupTowers :: V.Vector Location -> M.Map Char [Integer]
groupTowers = V.ifoldr updateMap M.empty
  where
    updateMap _ Empty = id
    updateMap index (Tower c) = M.insertWith (++) c [toInteger index]

getAntinodes :: Integer -> Integer -> [Integer] -> [(Integer, Integer)]
getAntinodes width heigth ps =
  let pairs = filter (uncurry (/=)) $ (,) <$> ps <*> ps
   in filter inBounds $ singleAntinode <$> pairs
  where
    singleAntinode (pa, pb) = toCoords pa + (toCoords pa - toCoords pb)
    toCoords p = (p `mod` width, p `div` width)
    inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < heigth

uniqueAntinodes :: CityMap -> [(Integer, Integer)]
uniqueAntinodes (CityMap width heigth locations) =
  let towers = M.elems (groupTowers locations)
   in nub $ concatMap (getAntinodes width heigth) towers

getAntinodes' :: Integer -> Integer -> [Integer] -> [(Integer, Integer)]
getAntinodes' width heigth ps =
  let pairs = (,) <$> ps <*> ps
   in concatMap allAntinodes pairs
  where
    allAntinodes (pa, pb) =
      if pa == pb
        then [toCoords pa]
        else
          let delta = toCoords pa - toCoords pb
           in takeWhile inBounds $ iterate (+ delta) (toCoords pa)
    toCoords p = (p `mod` width, p `div` width)
    inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < heigth

uniqueAntinodes' :: CityMap -> [(Integer, Integer)]
uniqueAntinodes' (CityMap width heigth locations) =
  let towers = filter ((/= 1) . length) $ M.elems (groupTowers locations)
   in nub $ concatMap (getAntinodes' width heigth) towers

parse :: Text -> CityMap
parse input =
  let rows = lines input
      height = length rows
      width = maybe 0 T.length (viaNonEmpty head rows)
      locations = V.fromList . map toLocation . T.unpack . T.filter (/= '\n') $ input
   in CityMap (toInteger width) (toInteger height) locations

part1 :: Text -> Integer
part1 input =
  let cm = parse input
   in genericLength $ uniqueAntinodes cm

part2 :: Text -> Integer
part2 input =
  let cm = parse input
   in genericLength $ uniqueAntinodes' cm

day8 :: Text -> IO (String, String)
day8 input = do
  return (show $ part1 input, show $ part2 input)
