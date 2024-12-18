module Day16
  ( day16,
  )
where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Array.ST
import Data.Bits
import qualified Data.List as L
import Data.STRef
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data Tile = Empty | Wall | Visited | Start | End
  deriving (Ord, Eq, Show)

data Dir = S | E | N | W
  deriving (Eq, Ord, Show, Bounded, Enum)

reachable :: Dir -> Dir -> Bool
reachable N S = False
reachable S N = False
reachable E W = False
reachable W E = False
reachable _ _ = True

cw :: Dir -> Dir
cw N = E
cw E = S
cw S = W
cw W = N

ccw :: Dir -> Dir
ccw N = W
ccw W = S
ccw S = E
ccw E = N

isCW :: Dir -> Dir -> Bool
isCW N E = True
isCW E S = True
isCW S W = True
isCW W N = True
isCW N W = False
isCW W S = False
isCW S E = False
isCW E N = False

moveCoords :: Dir -> (Integer, Integer) -> (Integer, Integer)
moveCoords dir (x, y) =
  case dir of
    E -> (x + 1, y)
    S -> (x, y + 1)
    W -> (x - 1, y)
    N -> (x, y - 1)

unmoveCoords :: (Integer, Integer) -> (Integer, Integer) -> Dir
unmoveCoords (x, y) (x', y')
  | x' == x + 1 = E
  | y' == y + 1 = S
  | x' == x - 1 = W
  | y' == y - 1 = N

data Reindeer = Reindeer
  { rCoords :: (Integer, Integer),
    rDir :: Dir
  }
  deriving (Eq, Ord, Show)

data RaindeerMaze = RaindeerMaze
  { mWidth :: Integer,
    mHeight :: Integer,
    mTiles :: V.Vector Tile,
    mReindeer :: Reindeer
  }
  deriving (Ord, Eq, Show)

parseRaindeerMaze :: Text -> RaindeerMaze
parseRaindeerMaze input =
  let rows = lines input
      height = length rows
      width = maybe 0 T.length (viaNonEmpty head rows)
      tiles = V.fromList . map toTile . T.unpack . T.filter (/= '\n') $ input
      rP = fromMaybe 0 (V.elemIndex Start tiles)
      rX = toInteger $ rP `mod` width
      rY = toInteger $ rP `div` width
   in RaindeerMaze (toInteger width) (toInteger height) tiles (Reindeer (rX, rY) E)
  where
    toTile '.' = Empty
    toTile '#' = Wall
    toTile 'S' = Start
    toTile 'E' = End

shortestPath :: RaindeerMaze -> (Integer, Integer, [Text])
shortestPath (RaindeerMaze width height tiles reindeer) = runST $ do
  -- the map
  tilesArr <- newListArray (0, V.length tiles - 1) (toChar <$> V.toList tiles) :: ST s (STUArray s Int Char)
  -- the result structures
  scoresArr <- newArray (0, V.length tiles * 4 - 1) (-1) :: ST s (STUArray s Int Int)
  parentsArr <- newArray (0, V.length tiles * 4 - 1) 0 :: ST s (STUArray s Int Int8)
  -- iteration structures
  candidatesArr <- newArray (0, V.length tiles * 4 - 1) 0 :: ST s (STUArray s Int Int)
  candidatesRef <- newSTRef 0
  doneArr <- newArray (0, V.length tiles * 4 - 1) False :: ST s (STUArray s Int Bool)
  -- set initial node
  writeArray scoresArr (toInt reindeer) 0
  stAdd candidatesRef candidatesArr reindeer

  whileM_ (not <$> stEmpty candidatesRef) $ do
    -- pick next best candidate
    currReindeer <- stFindNext candidatesRef candidatesArr scoresArr
    -- check its neighbors. if they have not been visited, set their score, and add them to the list
    mapM_ (stCheckNeighbor tilesArr candidatesRef candidatesArr doneArr scoresArr parentsArr currReindeer) [E, S, W, N]
    -- mark it as visited
    writeArray doneArr (toInt currReindeer) True

  -- compute the results
  let endIdx = fromMaybe 0 $ V.findIndex (== End) tiles
      endX = toInteger $ endIdx `mod` fromInteger width
      endY = toInteger $ endIdx `div` fromInteger width

  -- there are four end nodes, one for each direction
  ends <- mapM (readArray scoresArr . toInt . Reindeer (endX, endY)) [E, S, W, N]
  let minScore = L.minimum $ L.filter (> 0) ends
  parents <- mapM (stParents parentsArr . toInt . Reindeer (endX, endY)) [E, S, W, N]
  let tiles = L.nub . concatMap fst . filter ((== minScore) . snd) $ zip parents ends

  -- render final maze
  forM_ tiles $ \idx -> do
    writeArray tilesArr (fromInteger idx) 'O'
  raw <- getElems tilesArr
  let ascii = T.chunksOf (fromInteger width) $ T.pack raw

  return (toInteger minScore, L.genericLength tiles, ascii)
  where
    toChar Empty = '.'
    toChar Wall = '#'
    toChar Start = '.'
    toChar End = '.'

    fromInt i =
      let d = toEnum $ i `mod` 4 :: Dir
          c = i `div` 4
          x = c `mod` fromInteger width
          y = c `div` fromInteger width
       in Reindeer (toInteger x, toInteger y) d
    toInt (Reindeer (x, y) dir) = fromInteger (y * width + x) * 4 + fromEnum dir

    stEmpty :: STRef s Int -> ST s Bool
    stEmpty candidatesRef = do
      lastCandidate <- readSTRef candidatesRef
      return $ lastCandidate == 0

    stAdd :: STRef s Int -> STUArray s Int Int -> Reindeer -> ST s ()
    stAdd candidatesRef candidatesArr reindeer = do
      lastCandidate <- readSTRef candidatesRef
      writeArray candidatesArr lastCandidate (toInt reindeer)
      modifySTRef' candidatesRef succ

    stFindNext :: STRef s Int -> STUArray s Int Int -> STUArray s Int Int -> ST s Reindeer
    stFindNext candidatesRef candidatesArr scoresArr = do
      lastCandidate <- readSTRef candidatesRef
      candidates <- mapM (readArray candidatesArr) [0 .. lastCandidate - 1]
      min <- minimumOnM (readArray scoresArr) candidates
      let idx = fromMaybe 0 min
          candidateIdx = fromMaybe 0 $ L.elemIndex idx candidates
      -- remove candidate by replacing it with the last one and shortening the list
      last <- readArray candidatesArr (lastCandidate - 1)
      writeArray candidatesArr candidateIdx last
      modifySTRef' candidatesRef pred
      return $ fromInt idx

    stCheckNeighbor :: STUArray s Int Char -> STRef s Int -> STUArray s Int Int -> STUArray s Int Bool -> STUArray s Int Int -> STUArray s Int Int8 -> Reindeer -> Dir -> ST s ()
    stCheckNeighbor tilesArr candidatesRef candidatesArr doneArr scoresArr parentsArr currReindeer@(Reindeer (x, y) dir) dir' = do
      let (x', y') = moveCoords dir' (x, y)
      tile <- readArray tilesArr (fromInteger $ y' * width + x')
      when (tile == '.' && reachable dir dir') $ do
        -- are we walking or are we rotating?
        let nextReindeer =
              if dir == dir'
                then Reindeer (x', y') dir'
                else Reindeer (x, y) dir'
        done <- readArray doneArr (toInt nextReindeer)
        unless done $ do
          stUpdateScore scoresArr parentsArr currReindeer nextReindeer
          stAdd candidatesRef candidatesArr nextReindeer

    stUpdateScore :: STUArray s Int Int -> STUArray s Int Int8 -> Reindeer -> Reindeer -> ST s ()
    stUpdateScore scoresArr parentsArr currReindeer nextReindeer = do
      currScore <- readArray scoresArr (toInt currReindeer)
      oldScore <- readArray scoresArr (toInt nextReindeer)
      let newScore = currScore + fromInteger (moveScore currReindeer nextReindeer)
      when (oldScore == -1 || newScore <= oldScore) $ do
        writeArray scoresArr (toInt nextReindeer) newScore
        -- update parents bitmask
        oldParents <- readArray parentsArr (toInt nextReindeer)
        writeArray parentsArr (toInt nextReindeer) (oldParents .|. moveParent currReindeer nextReindeer)

    moveScore :: Reindeer -> Reindeer -> Integer
    moveScore (Reindeer _ dir) (Reindeer _ dir') = if dir == dir' then 1 else 1000

    moveParent :: Reindeer -> Reindeer -> Int8
    moveParent (Reindeer (x, y) dir) (Reindeer (x', y') dir')
      | dir == dir' = case unmoveCoords (x, y) (x', y') of
          S -> 1
          E -> 2
          N -> 4
          W -> 8
      | isCW dir dir' = 16
      | otherwise = 32

    stParents :: STUArray s Int Int8 -> Int -> ST s [Integer]
    stParents parentsArr idx = do
      parents <- readArray parentsArr idx

      let (Reindeer (x, y) dir) = fromInt idx
          pS = if parents .&. 1 > 0 then Just (Reindeer (x, y - 1) dir) else Nothing
          pE = if parents .&. 2 > 0 then Just (Reindeer (x - 1, y) dir) else Nothing
          pN = if parents .&. 4 > 0 then Just (Reindeer (x, y + 1) dir) else Nothing
          pW = if parents .&. 8 > 0 then Just (Reindeer (x + 1, y) dir) else Nothing
          pCW = if parents .&. 16 > 0 then Just (Reindeer (x, y) (ccw dir)) else Nothing
          pCCW = if parents .&. 32 > 0 then Just (Reindeer (x, y) (cw dir)) else Nothing
      parentTiles <- mapM (stParents parentsArr) $ toInt <$> catMaybes [pS, pE, pN, pW, pCW, pCCW]
      return $ y * width + x : L.concat parentTiles

day16 :: Text -> IO (String, String)
day16 input = do
  let rm = parseRaindeerMaze input
      (minScore, bestTiles, maze) = shortestPath rm
  mapM_ (putStrLn . T.unpack) maze
  return (show minScore, show bestTiles)
