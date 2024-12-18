module Day18
  ( day18,
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
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Relude

newtype Byte = Byte (Integer, Integer)
  deriving (Eq, Ord, Show)

data Memory = Safe | Corrupted
  deriving (Eq, Ord, Show)

data Dir = S | E | N | W
  deriving (Eq, Ord, Show, Bounded, Enum)

move :: Dir -> (Integer, Integer) -> (Integer, Integer)
move dir (x, y) =
  case dir of
    E -> (x + 1, y)
    S -> (x, y + 1)
    W -> (x - 1, y)
    N -> (x, y - 1)

unmove :: (Integer, Integer) -> (Integer, Integer) -> Dir
unmove (x, y) (x', y')
  | x' == x + 1 = E
  | y' == y + 1 = S
  | x' == x - 1 = W
  | y' == y - 1 = N

memorySize :: Integer
memorySize = 71 -- 0 to 70

valid :: (Integer, Integer) -> Bool
valid (x, y) = x >= 0 && x < memorySize && y >= 0 && y < memorySize

parseInput :: Text -> [Byte]
parseInput input = parseByte <$> lines input
  where
    parseByte str =
      let [x, y] = T.splitOn "," str
       in Byte (unsafeParseInteger x, unsafeParseInteger y)
    unsafeParseInteger str =
      case TR.decimal str of
        Right (i, _) -> i
        Left _ -> undefined

simulate :: [Byte] -> Integer -> V.Vector Memory
simulate bs n =
  let emptyMemory = V.replicate (fromInteger $ memorySize * memorySize) Safe
      corrupted = corryptedByte <$> take (fromInteger n) bs
   in emptyMemory V.// corrupted
  where
    corryptedByte :: Byte -> (Int, Memory)
    corryptedByte (Byte (x, y)) = (fromInteger $ y * memorySize + x, Corrupted)

display :: V.Vector Memory -> [Text]
display v =
  let txt = T.pack $ toChar <$> V.toList v
   in T.chunksOf (fromInteger memorySize) txt
  where
    toChar Safe = '.'
    toChar Corrupted = '#'

tag :: (Show a) => String -> a -> a
tag label value = trace (label ++ show value) value

shortestPath :: V.Vector Memory -> Integer
shortestPath memory = runST $ do
  -- the map
  tilesArr <- newListArray (0, V.length memory - 1) (toChar <$> V.toList memory) :: ST s (STUArray s Int Char)
  -- the result structures
  scoresArr <- newArray (0, V.length memory - 1) (-1) :: ST s (STUArray s Int Int)
  parentsArr <- newArray (0, V.length memory - 1) 0 :: ST s (STUArray s Int Int8)
  -- iteration structures
  candidatesArr <- newArray (0, V.length memory - 1) 0 :: ST s (STUArray s Int Int)
  candidatesRef <- newSTRef 0
  doneArr <- newArray (0, V.length memory - 1) False :: ST s (STUArray s Int Bool)
  -- set initial node
  writeArray scoresArr (toInt (0, 0)) 0
  stAdd candidatesRef candidatesArr (0, 0)

  whileM_ (not <$> stEmpty candidatesRef) $ do
    -- pick next best candidate
    currPos <- stFindNext candidatesRef candidatesArr scoresArr
    -- check its neighbors. if they have not been visited, set their score, and add them to the list
    mapM_ (stCheckNeighbor tilesArr candidatesRef candidatesArr doneArr scoresArr parentsArr currPos) [E, S, W, N]
    -- mark it as visited
    writeArray doneArr (toInt currPos) True

  minScore <- readArray scoresArr (toInt (memorySize - 1, memorySize - 1))

  return $ toInteger minScore
  where
    toChar Safe = '.'
    toChar Corrupted = '#'

    toInt (x, y) = fromInteger $ y * memorySize + x

    fromInt i =
      let x = i `mod` fromInteger memorySize
          y = i `div` fromInteger memorySize
       in (toInteger x, toInteger y)

    stEmpty :: STRef s Int -> ST s Bool
    stEmpty candidatesRef = do
      lastCandidate <- readSTRef candidatesRef
      return $ lastCandidate == 0

    stAdd :: STRef s Int -> STUArray s Int Int -> (Integer, Integer) -> ST s ()
    stAdd candidatesRef candidatesArr pos = do
      lastCandidate <- readSTRef candidatesRef
      writeArray candidatesArr lastCandidate (toInt $ tag "Added Candidate: " pos)
      modifySTRef' candidatesRef succ

    stFindNext :: STRef s Int -> STUArray s Int Int -> STUArray s Int Int -> ST s (Integer, Integer)
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

    stCheckNeighbor :: STUArray s Int Char -> STRef s Int -> STUArray s Int Int -> STUArray s Int Bool -> STUArray s Int Int -> STUArray s Int Int8 -> (Integer, Integer) -> Dir -> ST s ()
    stCheckNeighbor tilesArr candidatesRef candidatesArr doneArr scoresArr parentsArr currPos dir = do
      let nextPos = move dir currPos
      when (valid nextPos) $ do
        tile <- readArray tilesArr (toInt nextPos)
        when (tile == '.') $ do
          done <- readArray doneArr (toInt nextPos)
          unless done $ do
            stUpdateScore scoresArr parentsArr currPos nextPos
            stAdd candidatesRef candidatesArr nextPos

    stUpdateScore :: STUArray s Int Int -> STUArray s Int Int8 -> (Integer, Integer) -> (Integer, Integer) -> ST s ()
    stUpdateScore scoresArr parentsArr currPos nextPos = do
      currScore <- readArray scoresArr (toInt currPos)
      oldScore <- readArray scoresArr (toInt nextPos)
      let newScore = currScore + 1
      when (oldScore == -1 || newScore <= oldScore) $ do
        writeArray scoresArr (toInt nextPos) newScore
        -- update parents bitmask
        oldParents <- readArray parentsArr (toInt nextPos)
        writeArray parentsArr (toInt nextPos) (oldParents .|. moveParent currPos nextPos)

    moveParent :: (Integer, Integer) -> (Integer, Integer) -> Int8
    moveParent (x, y) (x', y') =
      case unmove (x, y) (x', y') of
        S -> 1
        E -> 2
        N -> 4
        W -> 8

    stParents :: STUArray s Int Int8 -> Int -> ST s [Integer]
    stParents parentsArr idx = do
      parents <- readArray parentsArr idx

      let (x, y) = fromInt idx
          pS = if parents .&. 1 > 0 then Just (x, y - 1) else Nothing
          pE = if parents .&. 2 > 0 then Just (x - 1, y) else Nothing
          pN = if parents .&. 4 > 0 then Just (x, y + 1) else Nothing
          pW = if parents .&. 8 > 0 then Just (x + 1, y) else Nothing
      parentTiles <- mapM (stParents parentsArr) $ toInt <$> catMaybes [pS, pE, pN, pW]
      return $ y * memorySize + x : concat parentTiles

day18 :: Text -> IO (String, String)
day18 input = do
  let bs = parseInput input
      mem = simulate bs 1024
      txt = display mem
  mapM_ (putStrLn . T.unpack) txt
  return (show $ shortestPath mem, "N/A")
