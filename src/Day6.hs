module Day6
  ( day6,
  )
where

import Control.Monad.Loops (untilM_)
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Map.Strict as M
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data Tile = Empty | Obstacle | Visited
  deriving (Ord, Eq, Show)

toChar :: Tile -> Char
toChar t =
  case t of
    Empty -> 'E'
    Obstacle -> 'O'
    Visited -> 'V'

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
isLoop (LabMap width heigth tiles guard) =
  runST $ do
    guardRef <- newSTRef guard
    mapRef <- newSTRef M.empty
    tilesArr <- newListArray (0, V.length tiles - 1) (toChar <$> V.toList tiles) :: ST s (STUArray s Int Char)
    indexRef <- newSTRef 0
    untilM_ (stStep width heigth guardRef tilesArr) (stDone guardRef mapRef indexRef)
    guard <- readSTRef guardRef
    return (gDir guard /= OOB)

stStep :: Integer -> Integer -> STRef s Guard -> STUArray s Int Char -> ST s ()
stStep width heigth guardRef tilesArr = do
  guard <- readSTRef guardRef
  let (x', y') = gCoords $ advance guard
  if x' >= 0 && x' < width && y' >= 0 && y' < heigth
    then do
      tile <- readArray tilesArr $ toIndex (x', y')
      case tile of
        'O' -> do
          writeSTRef guardRef (rotate guard)
          return ()
        _ -> do
          writeSTRef guardRef (advance guard)
          writeArray tilesArr (toIndex (x', y')) 'V'
          return ()
    else do
      writeSTRef guardRef (guard {gDir = OOB})
      return ()
  where
    toIndex (x, y) = fromIntegral $ y * width + x

stDone :: STRef s Guard -> STRef s (M.Map Guard Integer) -> STRef s Integer -> ST s Bool
stDone guardRef mapRef indexRef = do
  guard <- readSTRef guardRef
  if gDir guard == OOB
    then return True
    else do
      map <- readSTRef mapRef
      case guard `M.lookup` map of
        Just iv -> return True
        Nothing -> do
          index <- readSTRef indexRef
          modifySTRef' mapRef (M.insert guard index)
          modifySTRef' indexRef (+ 1)
          return False

part2 :: Text -> Integer
part2 input =
  let lm = parseLabMap input
      lms = iterate step lm
      lastLabs = filter ((== OOB) . gDir . mGuard) lms
      Just lastLab = viaNonEmpty head lastLabs
      positions = getVisited' lastLab
   in genericLength $ filter isLoop $ putObstacle' lm <$> positions

day6 :: Text -> IO (String, String)
day6 input = do
  return (show $ part1 input, show $ part2 input)
