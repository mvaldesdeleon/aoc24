module Day15
  ( day15,
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Array.ST
import Data.Attoparsec.Text
import qualified Data.List as L
import Data.STRef
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude
import System.Console.ANSI

data Tile = EmptyTile | BoxTile | WallTile
  deriving (Eq, Ord, Show)

charToTile :: Char -> Tile
charToTile '.' = EmptyTile
charToTile 'O' = BoxTile
charToTile '#' = WallTile
charToTile '@' = EmptyTile

tileToChar :: Tile -> Char
tileToChar EmptyTile = '.'
tileToChar BoxTile = 'O'
tileToChar WallTile = '#'

data WideTile = EmptyWideTile | BoxLeftWideTile Integer | BoxRightWideTile Integer | WallWideTile | RobotTile
  deriving (Eq, Ord, Show)

charToWideTiles :: Char -> [WideTile]
charToWideTiles '.' = [EmptyWideTile, EmptyWideTile]
charToWideTiles 'O' = [EmptyWideTile, EmptyWideTile]
charToWideTiles '#' = [WallWideTile, WallWideTile]
charToWideTiles '@' = [EmptyWideTile, EmptyWideTile]

wideTileToChar :: WideTile -> Char
wideTileToChar EmptyWideTile = '.'
wideTileToChar (BoxLeftWideTile _) = '['
wideTileToChar (BoxRightWideTile _) = ']'
wideTileToChar WallWideTile = '#'
wideTileToChar RobotTile = '@'

wideTileToInt :: WideTile -> Int
wideTileToInt EmptyWideTile = -1
wideTileToInt (BoxLeftWideTile idx) = fromInteger (idx * 2)
wideTileToInt (BoxRightWideTile idx) = fromInteger (idx * 2 + 1)
wideTileToInt WallWideTile = -2

intToWideTile :: Int -> WideTile
intToWideTile (-1) = EmptyWideTile
intToWideTile (-2) = WallWideTile
intToWideTile idx =
  let idx' = toInteger $ idx `div` 2
   in if even idx then BoxLeftWideTile idx' else BoxRightWideTile idx'

clearBoxes :: WideTile -> WideTile
clearBoxes (BoxLeftWideTile _) = EmptyWideTile
clearBoxes (BoxRightWideTile _) = EmptyWideTile
clearBoxes wt = wt

newtype Robot = Robot
  { unRobot :: (Integer, Integer)
  }
  deriving (Eq, Ord, Show)

data Dir = N | E | S | W
  deriving (Eq, Ord, Show, Bounded, Enum)

charToDir :: Char -> Dir
charToDir '^' = N
charToDir '>' = E
charToDir 'v' = S
charToDir '<' = W

data WarehouseMap = WarehouseMap
  { mWidth :: Integer,
    mHeight :: Integer,
    mTiles :: V.Vector Tile,
    mRobot :: Robot,
    mMoves :: [Dir]
  }
  deriving (Ord, Eq, Show)

newtype Box = Box
  { unBox :: (Integer, Integer)
  }
  deriving (Eq, Ord, Show)

boxToInt :: Box -> Int
boxToInt (Box (x, y)) = fromInteger $ y * 100 + x

intToBox :: Int -> Box
intToBox i = Box (toInteger $ i `mod` 100, toInteger $ i `div` 100)

data WideWarehouseMap = WideWarehouseMap
  { wmWidth :: Integer,
    wmHeight :: Integer,
    wmTiles :: V.Vector WideTile,
    wmBoxes :: V.Vector Box,
    wmRobot :: Robot,
    wmMoves :: [Dir]
  }
  deriving (Ord, Eq, Show)

parseDirs :: Parser [Dir]
parseDirs = map charToDir <$> many1 (notChar '\n')

parseMap :: Parser (Integer, Integer, V.Vector Tile, Robot)
parseMap = buildMap . concat <$> many1 parseLine
  where
    parseLine = addNewline <$> many1 (notChar '\n') <* endOfLine
    addNewline str = str ++ "\n"

parseWideMap :: Parser (Integer, Integer, V.Vector WideTile, V.Vector Box, Robot)
parseWideMap = buildWideMap . concat <$> many1 parseLine
  where
    parseLine = addNewline <$> many1 (notChar '\n') <* endOfLine
    addNewline str = str ++ "\n"

buildMap :: [Char] -> (Integer, Integer, V.Vector Tile, Robot)
buildMap str =
  let rows = lines (T.pack str)
      height = length rows - 1
      width = maybe 0 T.length (viaNonEmpty head rows)
      raw = filter ((/=) '\n') str
      Just ridx = L.elemIndex '@' raw
   in (toInteger width, toInteger height, V.fromList $ charToTile <$> raw, Robot (toInteger $ ridx `mod` width, toInteger $ ridx `div` width))

buildWideMap :: [Char] -> (Integer, Integer, V.Vector WideTile, V.Vector Box, Robot)
buildWideMap str =
  let rows = lines (T.pack str)
      height = toInteger $ length rows - 1
      width = toInteger $ maybe 0 T.length (viaNonEmpty head rows)
      toCoords idx = (toInteger $ 2 * (idx `mod` width), toInteger $ idx `div` width)
      raw = filter ((/=) '\n') str
      Just ridx = toInteger <$> L.elemIndex '@' raw
      bidxs = L.elemIndices 'O' raw
      boxes = V.fromList $ Box . toCoords . toInteger <$> bidxs
      tiles = V.fromList $ concatMap charToWideTiles raw
   in (2 * width, toInteger height, updateWithBoxes (2 * width) boxes tiles, boxes, Robot $ toCoords ridx)

parseWarehouseMap :: Parser WarehouseMap
parseWarehouseMap = do
  (width, height, tiles, robot) <- parseMap
  endOfLine
  dirss <- parseDirs `sepBy` endOfLine
  return $ WarehouseMap width height tiles robot (concat dirss)

parseWideWarehouseMap :: Parser WideWarehouseMap
parseWideWarehouseMap = do
  (width, height, wtiles, boxes, robot) <- parseWideMap
  endOfLine
  dirss <- parseDirs `sepBy` endOfLine
  return $ WideWarehouseMap width height wtiles boxes robot (concat dirss)

simulate :: WarehouseMap -> WarehouseMap
simulate (WarehouseMap width height tiles robot moves) = runST $ do
  robotRef <- newSTRef robot
  tilesArr <- newListArray (0, V.length tiles - 1) (tileToChar <$> V.toList tiles) :: ST s (STUArray s Int Char)
  mapM_ (stTryMove width robotRef tilesArr) moves
  tiles' <- map charToTile <$> getElems tilesArr
  robot' <- readSTRef robotRef
  return $ WarehouseMap width height (V.fromList tiles') robot' []

moveRobot :: Dir -> Robot -> Robot
moveRobot dir = Robot . moveCoords dir . unRobot

moveBox :: Dir -> Box -> Box
moveBox dir = Box . moveCoords dir . unBox

moveCoords :: Dir -> (Integer, Integer) -> (Integer, Integer)
moveCoords dir (x, y) =
  case dir of
    N -> (x, y - 1)
    E -> (x + 1, y)
    S -> (x, y + 1)
    W -> (x - 1, y)

stTryMove :: Integer -> STRef s Robot -> STUArray s Int Char -> Dir -> ST s ()
stTryMove width robotRef tilesArr move = do
  robot <- readSTRef robotRef
  let (rx', ry') = unRobot $ moveRobot move robot
  next <- readArray tilesArr (fromInteger $ ry' * width + rx')
  case next of
    '#' -> return ()
    '.' -> writeSTRef robotRef $ Robot (rx', ry')
    'O' -> stTryPush width robotRef tilesArr move

stTryPush :: Integer -> STRef s Robot -> STUArray s Int Char -> Dir -> ST s ()
stTryPush width robotRef tilesArr move = do
  robot <- readSTRef robotRef
  let positions = L.drop 1 $ unRobot <$> L.iterate (moveRobot move) robot
  boxes <- takeWhileM (stIsBox width tilesArr) positions
  let (lx, ly) = positions L.!! length boxes
  last <- readArray tilesArr (fromInteger $ ly * width + lx)
  case last of
    '#' -> return ()
    '.' -> do
      let robot' = moveRobot move robot
      let (sx, sy) = unRobot robot'
      writeArray tilesArr (fromInteger $ sy * width + sx) '.'
      writeArray tilesArr (fromInteger $ ly * width + lx) 'O'
      writeSTRef robotRef robot'

stIsBox :: Integer -> STUArray s Int Char -> (Integer, Integer) -> ST s Bool
stIsBox width tilesArr (x, y) = (==) 'O' <$> readArray tilesArr (fromInteger $ y * width + x)

boxCoords :: WarehouseMap -> [Integer]
boxCoords (WarehouseMap width _ tiles _ _) =
  let idxs = V.elemIndices BoxTile tiles
   in V.toList $ toGPS <$> idxs
  where
    toGPS :: Int -> Integer
    toGPS idx =
      let x = idx `mod` fromInteger width
          y = idx `div` fromInteger width
       in toInteger $ y * 100 + x

part1 :: Text -> Integer
part1 input =
  let wm = case parseOnly parseWarehouseMap input of
        Left err -> error $ toText err
        Right wm -> wm
   in sum $ boxCoords (simulate wm)

updateWithBoxes :: Integer -> V.Vector Box -> V.Vector WideTile -> V.Vector WideTile
updateWithBoxes width boxes tiles = runST $ do
  tilesArr <- newListArray (0, V.length tiles - 1) (wideTileToInt . clearBoxes <$> V.toList tiles) :: ST s (STUArray s Int Int)
  mapM_ (addBox tilesArr) (zip (V.toList boxes) [0 ..])
  wts <- map intToWideTile <$> getElems tilesArr
  return $ V.fromList wts
  where
    addBox :: STUArray s Int Int -> (Box, Integer) -> ST s ()
    addBox tilesArr (Box (x, y), idx) = do
      writeArray tilesArr (fromInteger $ y * width + x) (wideTileToInt $ BoxLeftWideTile idx)
      writeArray tilesArr (fromInteger $ y * width + x + 1) (wideTileToInt $ BoxRightWideTile idx)

simulateWide :: WideWarehouseMap -> WideWarehouseMap
simulateWide (WideWarehouseMap width height tiles boxes robot moves) = runST $ do
  robotRef <- newSTRef robot
  tilesArr <- newListArray (0, V.length tiles - 1) (wideTileToInt <$> V.toList tiles) :: ST s (STUArray s Int Int)
  boxesArr <- newListArray (0, V.length boxes - 1) (boxToInt <$> V.toList boxes) :: ST s (STUArray s Int Int)
  mapM_ (stTryMoveWide width robotRef tilesArr boxesArr) moves
  tiles' <- map intToWideTile <$> getElems tilesArr
  boxes' <- map intToBox <$> getElems boxesArr
  robot' <- readSTRef robotRef
  return $ WideWarehouseMap width height (V.fromList tiles') (V.fromList boxes') robot' []

simulateWide' :: WideWarehouseMap -> WideWarehouseMap
simulateWide' (WideWarehouseMap width height tiles boxes robot []) = WideWarehouseMap width height tiles boxes robot []
simulateWide' (WideWarehouseMap width height tiles boxes robot (m : ms)) =
  let wwm = simulateWide (WideWarehouseMap width height tiles boxes robot [m])
   in wwm {wmMoves = ms}

stTryMoveWide :: Integer -> STRef s Robot -> STUArray s Int Int -> STUArray s Int Int -> Dir -> ST s ()
stTryMoveWide width robotRef tilesArr boxesArr move = do
  robot <- readSTRef robotRef
  let (rx', ry') = unRobot $ moveRobot move robot
  next <- readArray tilesArr (fromInteger $ ry' * width + rx')
  case next of
    (-2) -> return ()
    (-1) -> writeSTRef robotRef $ Robot (rx', ry')
    _ -> stTryPushWide width robotRef tilesArr boxesArr move

stTryPushWide :: Integer -> STRef s Robot -> STUArray s Int Int -> STUArray s Int Int -> Dir -> ST s ()
stTryPushWide width robotRef tilesArr boxesArr move = do
  robot <- readSTRef robotRef
  res <- stTryPushBoxes width tilesArr (unRobot robot) move
  case res of
    Nothing -> return ()
    Just ids -> do
      stClearBoxes width tilesArr boxesArr
      let ids' = L.nub ids
      mapM_ (stMoveBox boxesArr move) ids'
      stAddBoxes width tilesArr boxesArr
      let robot' = moveRobot move robot
      writeSTRef robotRef robot'

stAddBoxes :: Integer -> STUArray s Int Int -> STUArray s Int Int -> ST s ()
stAddBoxes width tilesArr boxesArr = do
  boxes <- getElems boxesArr
  mapM_ (stAddBox width tilesArr) (zip (intToBox <$> boxes) [0 ..])

stClearBoxes :: Integer -> STUArray s Int Int -> STUArray s Int Int -> ST s ()
stClearBoxes width tilesArr boxesArr = do
  boxes <- getElems boxesArr
  mapM_ (stClearBox width tilesArr) (intToBox <$> boxes)

stClearBox :: Integer -> STUArray s Int Int -> Box -> ST s ()
stClearBox width tilesArr (Box (x, y)) = do
  writeArray tilesArr (fromInteger $ y * width + x) (-1)
  writeArray tilesArr (fromInteger $ y * width + x + 1) (-1)

stAddBox :: Integer -> STUArray s Int Int -> (Box, Integer) -> ST s ()
stAddBox width tilesArr (Box (x, y), idx) = do
  writeArray tilesArr (fromInteger $ y * width + x) (fromInteger $ idx * 2)
  writeArray tilesArr (fromInteger $ y * width + x + 1) (fromInteger $ idx * 2 + 1)

stMoveBox :: STUArray s Int Int -> Dir -> Integer -> ST s ()
stMoveBox boxesArr move bid = do
  box <- intToBox <$> readArray boxesArr (fromInteger bid)
  writeArray boxesArr (fromInteger bid) (boxToInt $ moveBox move box)

stTryPushBoxes :: Integer -> STUArray s Int Int -> (Integer, Integer) -> Dir -> ST s (Maybe [Integer])
stTryPushBoxes width tilesArr (x, y) move = do
  let (rx', ry') = unRobot $ moveRobot move (Robot (x, y))
  tile <- readArray tilesArr (fromInteger $ ry' * width + rx')
  case tile of
    (-2) -> return Nothing
    (-1) -> return $ Just []
    idx -> do
      let idx' = toInteger $ idx `div` 2
          isLeft = even idx
      case move of
        N -> do
          restA <- stTryPushBoxes width tilesArr (rx', ry') move
          restB <- stTryPushBoxes width tilesArr (if isLeft then rx' + 1 else rx' - 1, ry') move
          let rest = (++) <$> restA <*> restB
          return $ (idx' :) <$> rest
        S -> do
          restA <- stTryPushBoxes width tilesArr (rx', ry') move
          restB <- stTryPushBoxes width tilesArr (if isLeft then rx' + 1 else rx' - 1, ry') move
          let rest = (++) <$> restA <*> restB
          return $ (idx' :) <$> rest
        E -> do
          rest <- stTryPushBoxes width tilesArr (rx' + 1, ry') move
          return $ (idx' :) <$> rest
        W -> do
          rest <- stTryPushBoxes width tilesArr (rx' - 1, ry') move
          return $ (idx' :) <$> rest

boxCoordsWide :: WideWarehouseMap -> [Integer]
boxCoordsWide (WideWarehouseMap _ _ _ boxes _ _) =
  V.toList $ toGPS <$> boxes
  where
    toGPS :: Box -> Integer
    toGPS (Box (x, y)) = toInteger $ y * 100 + x

part2 :: Text -> Integer
part2 input =
  let wwm = case parseOnly parseWideWarehouseMap input of
        Left err -> error $ toText err
        Right wm -> wm
   in sum $ boxCoordsWide (simulateWide wwm)

render :: WarehouseMap -> IO ()
render (WarehouseMap width _ tiles _ _) =
  let str = T.pack . map tileToChar . V.toList $ tiles
      ts = T.chunksOf (fromInteger width) str
   in mapM_ (putStrLn . T.unpack) ts

renderWide :: WideWarehouseMap -> IO ()
renderWide (WideWarehouseMap width _ tiles _ (Robot (x, y)) _) =
  let str = T.pack . map wideTileToChar . V.toList $ (tiles V.// [(fromInteger $ y * width + x, RobotTile)])
      ts = T.chunksOf (fromInteger width) str
   in mapM_ (putStrLn . T.unpack) ts

day15 :: Text -> IO (String, String)
day15 input = do
  let wwm = case parseOnly parseWideWarehouseMap input of
        Left err -> error $ toText err
        Right wm -> wm
  clearScreen
  setCursorPosition 0 0
  renderWide wwm
  putStrLn ""
  putStrLn "Get ready..."
  threadDelay 5000000
  foldM_ printFrame wwm (wmMoves wwm)
  return (show $ part1 input, show $ part2 input)
  where
    printFrame wwm _ = do
      setCursorPosition 0 0
      renderWide wwm
      threadDelay 1000
      let wwm' = simulateWide' wwm
      return wwm'
