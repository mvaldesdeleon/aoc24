module Day18
  ( day18,
  )
where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Array.ST
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

shortestPath :: V.Vector Memory -> Integer
shortestPath memory = runST $ do
  -- the map
  tilesArr <- newListArray (0, V.length memory - 1) (toChar <$> V.toList memory) :: ST s (STUArray s Int Char)
  -- the result structures
  scoresArr <- newArray (0, V.length memory - 1) (-1) :: ST s (STUArray s Int Int)
  -- iteration structures
  candidatesArr <- newArray (0, V.length memory - 1) False :: ST s (STUArray s Int Bool)
  doneArr <- newArray (0, V.length memory - 1) False :: ST s (STUArray s Int Bool)
  -- set initial node
  writeArray scoresArr (toInt (0, 0)) 0
  stAdd candidatesArr (0, 0)

  whileM_ (not <$> stEmpty candidatesArr) $ do
    -- pick next best candidate
    currPos <- stFindNext candidatesArr scoresArr
    -- check its neighbors. if they have not been visited, set their score, and add them to the list
    mapM_ (stCheckNeighbor tilesArr candidatesArr doneArr scoresArr currPos) [E, S, W, N]
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

    stEmpty :: STUArray s Int Bool -> ST s Bool
    stEmpty candidatesArr = do
      candidates <- getElems candidatesArr
      return $ not . or $ candidates

    stAdd :: STUArray s Int Bool -> (Integer, Integer) -> ST s ()
    stAdd candidatesArr pos = do
      writeArray candidatesArr (toInt pos) True

    stFindNext :: STUArray s Int Bool -> STUArray s Int Int -> ST s (Integer, Integer)
    stFindNext candidatesArr scoresArr = do
      candidates <- map fst . filter snd <$> getAssocs candidatesArr
      min <- minimumOnM (readArray scoresArr) candidates
      let idx = fromMaybe 0 min
      writeArray candidatesArr idx False
      return $ fromInt idx

    stCheckNeighbor :: STUArray s Int Char -> STUArray s Int Bool -> STUArray s Int Bool -> STUArray s Int Int -> (Integer, Integer) -> Dir -> ST s ()
    stCheckNeighbor tilesArr candidatesArr doneArr scoresArr currPos dir = do
      let nextPos = move dir currPos
      when (valid nextPos) $ do
        tile <- readArray tilesArr (toInt nextPos)
        when (tile == '.') $ do
          done <- readArray doneArr (toInt nextPos)
          unless done $ do
            stUpdateScore scoresArr currPos nextPos
            stAdd candidatesArr nextPos

    stUpdateScore :: STUArray s Int Int -> (Integer, Integer) -> (Integer, Integer) -> ST s ()
    stUpdateScore scoresArr currPos nextPos = do
      currScore <- readArray scoresArr (toInt currPos)
      oldScore <- readArray scoresArr (toInt nextPos)
      let newScore = currScore + 1
      when (oldScore == -1 || newScore <= oldScore) $ do
        writeArray scoresArr (toInt nextPos) newScore

part1 :: Text -> Integer
part1 input =
  let bs = parseInput input
      mem = simulate bs 1024
   in shortestPath mem

findFirstBadByte :: [Byte] -> Byte
findFirstBadByte bs = (bs L.!!) $ bsearch f [0 .. length bs - 1]
  where
    f :: Int -> Ordering
    f i =
      let r = shortestPath $ simulate bs (toInteger i)
       in if r == -1 then GT else LT

bsearch :: (a -> Ordering) -> [a] -> a
bsearch comp as =
  let s = 0
      e = length as - 1
   in go s e
  where
    go s e
      | s == e = as L.!! (s - 1)
      | otherwise =
          let m = (e - s) `div` 2 + s
              a = as L.!! m
           in case comp a of
                EQ -> a
                LT -> go (m + 1) e
                GT -> go s m

part2 :: Text -> Byte
part2 input =
  let bs = parseInput input
   in findFirstBadByte bs

day18 :: Text -> IO (String, String)
day18 input = do
  let bs = parseInput input
      mem = map toChar . V.toList $ simulate bs 1024
      ascii = T.chunksOf (fromInteger memorySize) $ T.pack mem

  mapM_ (putStrLn . T.unpack) ascii

  return (show $ part1 input, show $ part2 input)
  where
    toChar Safe = '.'
    toChar Corrupted = '#'
