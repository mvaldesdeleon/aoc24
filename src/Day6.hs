module Day6
  ( day6,
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data Tile = Empty | Obstacle | Visited
  deriving (Ord, Eq, Show)

data Dir = S | E | N | W | OOB
  deriving (Eq, Ord, Show)

cw :: Dir -> Dir
cw dir =
  case dir of
    S -> W
    E -> S
    N -> E
    W -> N
    OOB -> OOB

data Guard = Guard
  { gCoords :: (Integer, Integer),
    gDir :: Dir
  }
  deriving (Eq, Ord, Show)

data LabMap = LabMap
  { mWidth :: Integer,
    mHeight :: Integer,
    mTiles :: V.Vector Tile,
    mGuard :: Guard
  }
  deriving (Ord, Eq, Show)

toTile :: Char -> Tile
toTile '#' = Obstacle
toTile '^' = Visited
toTile _ = Empty

parseLabMap :: Text -> LabMap
parseLabMap input =
  let rows = lines input
      height = length rows
      width = maybe 0 T.length (viaNonEmpty head rows)
      tiles = V.fromList . map toTile . T.unpack . T.filter (/= '\n') $ input
      gP = fromMaybe 0 (V.elemIndex Visited tiles)
      gX = toInteger $ gP `mod` width
      gY = toInteger $ gP `div` width
   in LabMap (toInteger width) (toInteger height) tiles (Guard (gX, gY) N)

advance :: Guard -> Guard
advance (Guard (gX, gY) dir) =
  case dir of
    S -> Guard (gX, gY + 1) dir
    E -> Guard (gX + 1, gY) dir
    N -> Guard (gX, gY - 1) dir
    W -> Guard (gX - 1, gY) dir
    OOB -> Guard (gX, gY) dir

rotate :: Guard -> Guard
rotate g = g {gDir = cw $ gDir g}

step :: LabMap -> LabMap
step (LabMap width heigth tiles guard) =
  let (x', y') = gCoords $ advance guard
   in if x' >= 0 && x' < width && y' >= 0 && y' < heigth
        then case tiles V.! toIndex (x', y') of
          Obstacle -> LabMap width heigth tiles (rotate guard)
          _ -> LabMap width heigth (tiles V.// [(toIndex (x', y'), Visited)]) (advance guard)
        else LabMap width heigth tiles (guard {gDir = OOB})
  where
    toIndex (x, y) = fromIntegral $ y * width + x

countVisited :: LabMap -> Integer
countVisited lm = toInteger . V.length $ V.findIndices (== Visited) (mTiles lm)

getVisited' :: LabMap -> [Integer]
getVisited' lm = toInteger <$> V.toList (V.findIndices (== Visited) (mTiles lm))

part1 :: Text -> Integer
part1 input =
  let lm = parseLabMap input
      lms = iterate step lm
      lastLabs = filter ((== OOB) . gDir . mGuard) lms
      Just lastLab = viaNonEmpty head lastLabs
   in countVisited lastLab

putObstacle' :: LabMap -> Integer -> LabMap
putObstacle' lm p = lm {mTiles = mTiles lm V.// [(fromInteger p, Obstacle)]}

isLoop :: LabMap -> Bool
isLoop lm =
  let gs = mGuard <$> iterate step lm
   in case findCycle gs of
        Nothing -> False
        Just (offset, len) -> len /= 1

findCycle :: (Ord a) => [a] -> Maybe (Integer, Integer)
findCycle vs = go vs 0 M.empty
  where
    go :: (Ord a) => [a] -> Integer -> M.Map a Integer -> Maybe (Integer, Integer)
    go [] _ _ = Nothing
    go (v : vs) i vals =
      case v `M.lookup` vals of
        Just iv -> Just (iv, i - iv)
        Nothing -> go vs (i + 1) (M.insert v i vals)

part2 :: Text -> Integer
part2 input =
  let lm = parseLabMap input
      lms = iterate step lm
      lastLabs = filter ((== OOB) . gDir . mGuard) lms
      Just lastLab = viaNonEmpty head lastLabs
      positions = getVisited' lastLab
   in genericLength $ filter isLoop $ putObstacle' lm <$> positions

-- in genericLength $ filter isLoop $ (putObstacle' lm . (\p -> trace ("P: " ++ show p) p)) <$> positions

day6 :: Text -> IO (String, String)
day6 input = do
  return (show $ part1 input, show $ part2 input)
