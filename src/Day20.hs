{-# LANGUAGE FlexibleContexts #-}

module Day20
  ( day20,
  )
where

import Control.Monad.ST
import Data.Array.ST
import qualified Data.List as L
import qualified Data.List.Split as LS
import Data.NumInstances.Tuple
import Data.STRef
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data Tile = Empty | Wall
  deriving (Ord, Eq, Show)

data Dir = S | E | N | W
  deriving (Eq, Ord, Show, Bounded, Enum)

move :: Dir -> (Integer, Integer) -> (Integer, Integer)
move dir (x, y) =
  case dir of
    E -> (x + 1, y)
    S -> (x, y + 1)
    W -> (x - 1, y)
    N -> (x, y - 1)

data RaceTrack = RaceTrack
  { rtWidth :: Integer,
    rtHeight :: Integer,
    rtTiles :: V.Vector Tile,
    rsStart :: (Integer, Integer)
  }
  deriving (Ord, Eq, Show)

parseRaceTrack :: Text -> RaceTrack
parseRaceTrack input =
  let rows = extendRow <$> lines input
      width = maybe 0 T.length (viaNonEmpty head rows)
      height = length rows + 2
      extraRow = T.pack . L.replicate width $ '#'
      raw = T.unpack . T.filter (/= '\n') $ mconcat ([extraRow] ++ rows ++ [extraRow])
      tiles = V.fromList . map toTile $ raw
      start = fromMaybe 0 (L.elemIndex 'S' raw)
      startX = toInteger $ start `mod` width
      startY = toInteger $ start `div` width
   in RaceTrack (toInteger width) (toInteger height) tiles (startX, startY)
  where
    toTile '#' = Wall
    toTile _ = Empty
    extendRow row = "#" <> row <> "#"

findCheats :: RaceTrack -> [(Int, Int)]
findCheats (RaceTrack width height tiles start) = runST $ do
  tilesArr <- newListArray (0, V.length tiles - 1) (toBool <$> V.toList tiles) :: ST s (STUArray s Int Bool)
  timesArr <- newArray (0, V.length tiles - 1) (-1) :: ST s (STUArray s Int Int)
  cheatsArr <- newArray (0, (V.length tiles * 4) - 1) (-1) :: ST s (STUArray s Int Int)

  calculateTimes tilesArr timesArr
  calculateCheats tilesArr timesArr cheatsArr

  filter ((/=) (-1) . snd) <$> getAssocs cheatsArr
  where
    toBool Wall = False
    toBool Empty = True

    toIndex (x, y) = fromInteger $ y * width + x

    calculateTimes :: STUArray s Int Bool -> STUArray s Int Int -> ST s ()
    calculateTimes tilesArr timesArr = go start 0
      where
        go pos time = do
          writeArray timesArr (toIndex pos) time
          mapM_ tryMove [S, E, N, W]
          where
            tryMove dir = do
              let next = move dir pos
              tile <- readArray tilesArr (toIndex next)
              time' <- readArray timesArr (toIndex next)
              when (tile && time' == -1) $ go next (time + 1)

    calculateCheats :: STUArray s Int Bool -> STUArray s Int Int -> STUArray s Int Int -> ST s ()
    calculateCheats tilesArr timesArr cheatsArr = go start
      where
        go pos = do
          mapM_ tryCheat [S, E, N, W]

          mapM_ tryMove [S, E, N, W]
          where
            tryCheat dir = do
              -- check if its possible to cheat in a given direction
              let next = move dir pos
                  next' = move dir next
              tile <- readArray tilesArr (toIndex next)
              tile' <- readArray tilesArr (toIndex next')
              time <- readArray timesArr (toIndex pos)
              time' <- readArray timesArr (toIndex next')
              when (not tile && tile' && time' > time) $ do
                let timeSaved = time' - (time + 2)
                writeArray cheatsArr (toIndex' dir) timeSaved
            tryMove dir = do
              let next = move dir pos
              tile <- readArray tilesArr (toIndex next)
              time <- readArray timesArr (toIndex pos)
              time' <- readArray timesArr (toIndex next)
              when (tile && time' > time) $ go next
            toIndex' dir = toIndex pos * 4 + fromEnum dir

findCheats' :: RaceTrack -> [(Int, Int)]
findCheats' (RaceTrack width height tiles start) = runST $ do
  tilesArr <- newListArray (0, V.length tiles - 1) (toBool <$> V.toList tiles) :: ST s (STUArray s Int Bool)
  timesArr <- newArray (0, V.length tiles - 1) (-1) :: ST s (STUArray s Int Int)
  cheatsArr <- newArray (0, (V.length tiles * V.length tiles) - 1) (-1) :: ST s (STUArray s Int Int)

  calculateTimes tilesArr timesArr
  calculateCheats tilesArr timesArr cheatsArr

  filter ((/=) (-1) . snd) <$> getAssocs cheatsArr
  where
    toBool Wall = False
    toBool Empty = True

    toIndex (x, y) = fromInteger $ y * width + x

    calculateTimes :: STUArray s Int Bool -> STUArray s Int Int -> ST s ()
    calculateTimes tilesArr timesArr = go start 0
      where
        go pos time = do
          writeArray timesArr (toIndex pos) time
          mapM_ tryMove [S, E, N, W]
          where
            tryMove dir = do
              let next = move dir pos
              tile <- readArray tilesArr (toIndex next)
              time' <- readArray timesArr (toIndex next)
              when (tile && time' == -1) $ go next (time + 1)

    calculateCheats :: STUArray s Int Bool -> STUArray s Int Int -> STUArray s Int Int -> ST s ()
    calculateCheats tilesArr timesArr cheatsArr = go start
      where
        go pos = do
          mapM_ tryCheat [(dx, dy) | dx <- [-20 .. 20], dy <- [-20 .. 20], abs dx + abs dy <= 20]

          mapM_ tryMove [S, E, N, W]
          where
            tryCheat delta = do
              -- check if its possible to cheat in a given direction
              let next' = pos + delta
              when (isValid next') $ do
                tile' <- readArray tilesArr (toIndex next')
                time <- readArray timesArr (toIndex pos)
                time' <- readArray timesArr (toIndex next')
                when (tile' && time' > time) $ do
                  let timeSaved = time' - (time + distance delta)
                  writeArray cheatsArr (toIndex' next') timeSaved
            tryMove dir = do
              let next = move dir pos
              tile <- readArray tilesArr (toIndex next)
              time <- readArray timesArr (toIndex pos)
              time' <- readArray timesArr (toIndex next)
              when (tile && time' > time) $ go next
            distance (dx, dy) = fromInteger $ abs dx + abs dy
            toIndex' next = fromInteger $ toIndex pos * width * height + toIndex next
            isValid (x, y) = x >= 0 && x < width && y >= 0 && y < height

part1 :: Text -> Integer
part1 input =
  let rt = parseRaceTrack input
      cheats = findCheats rt
   in L.genericLength $ filter ((>= 100) . snd) cheats

part2 :: Text -> Integer
part2 input =
  let rt = parseRaceTrack input
      cheats = findCheats' rt
   in L.genericLength $ filter ((>= 100) . snd) cheats

day20 :: Text -> IO (String, String)
day20 input =
  return (show $ part1 input, show $ part2 input)
