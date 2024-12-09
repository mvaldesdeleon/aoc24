module Day9
  ( day9,
  )
where

import Control.Monad.Loops
import Control.Monad.ST
import Data.Array.ST
import qualified Data.List as L
import Data.STRef
import qualified Data.Text as T
import Relude hiding (empty)

data Block = Empty | File Integer
  deriving (Eq, Ord, Show)

data SizedBlock = SizedBlock Block Integer
  deriving (Eq, Ord, Show)

getId :: Block -> Integer
getId (File id) = id
getId Empty = 0

empty :: Block -> Bool
empty Empty = True
empty _ = False

empty' :: SizedBlock -> Bool
empty' (SizedBlock b _) = empty b

parse :: Text -> [Block]
parse = concatMap toBlock . zip [0 ..] . T.unpack
  where
    toBlock (index, c) =
      let size = fromEnum c - fromEnum '0'
          id = index `div` 2
          isFile = even index
          block = if isFile then File id else Empty
       in L.replicate size block

parse' :: Text -> ([SizedBlock], [SizedBlock])
parse' = L.partition (not . empty') . zipWith toBlock [0 ..] . T.unpack . (<> "0")
  where
    toBlock index c =
      let size = fromEnum c - fromEnum '0'
          id = index `div` 2
          isFile = even index
          block = if isFile then File id else Empty
       in SizedBlock block (toInteger size)

compress :: [Block] -> [Block]
compress bs =
  runST $ do
    blocks <- newListArray (0, length bs - 1) (fromInteger . toInt <$> bs) :: ST s (STUArray s Int Int)
    startRef <- newSTRef 0
    endRef <- newSTRef (length bs - 1)
    whileM_ (stNotDone startRef endRef blocks) (stSwap startRef endRef blocks)
    bs' <- getElems blocks
    return (fromInt . toInteger <$> bs')
  where
    toInt Empty = -1
    toInt (File id) = id
    fromInt (-1) = Empty
    fromInt id = File id

stSwap :: STRef s Int -> STRef s Int -> STUArray s Int Int -> ST s ()
stSwap startRef endRef blocks = do
  start <- readSTRef startRef
  end <- readSTRef endRef
  sb <- readArray blocks start
  eb <- readArray blocks end
  writeArray blocks start eb
  writeArray blocks end sb
  return ()

stNotDone :: STRef s Int -> STRef s Int -> STUArray s Int Int -> ST s Bool
stNotDone startRef endRef blocks = do
  start <- readSTRef startRef
  end <- readSTRef endRef
  whileM_ (not <$> isEmpty (start, end) startRef blocks) (modifySTRef' startRef succ)
  whileM_ (isEmpty (start, end) endRef blocks) (modifySTRef' endRef pred)
  start <- readSTRef startRef
  end <- readSTRef endRef
  return $ start < end

isEmpty :: (Int, Int) -> STRef s Int -> STUArray s Int Int -> ST s Bool
isEmpty (min, max) indexRef blocks = do
  index <- readSTRef indexRef
  if min <= index && index <= max
    then do
      b <- readArray blocks index
      return $ b == (-1)
    else return False

part1 :: Text -> Integer
part1 input =
  let bs = parse input
   in checksum $ compress bs

checksum :: [Block] -> Integer
checksum bs = sum $ uncurry (*) <$> zip [0 ..] (getId <$> bs)

data FilledEmpty = FilledEmpty Integer [SizedBlock]
  deriving (Eq, Ord, Show)

toFilledEmpty :: SizedBlock -> FilledEmpty
toFilledEmpty (SizedBlock Empty size) = FilledEmpty size []
toFilledEmpty (SizedBlock _ size) = undefined

toEmpty :: SizedBlock -> SizedBlock
toEmpty (SizedBlock Empty _) = undefined
toEmpty (SizedBlock _ size) = SizedBlock Empty size

data Compressed = Compressed [SizedBlock] [FilledEmpty]
  deriving (Eq, Ord, Show)

compress' :: ([SizedBlock], [SizedBlock]) -> [Block]
compress' (files, empties) =
  let revFiles = reverse files
      filledEmpties = toFilledEmpty <$> empties
      compressed = L.foldl moveFile (Compressed [] filledEmpties) revFiles
   in toBlocks compressed

toBlocks :: Compressed -> [Block]
toBlocks (Compressed files filledEmpties) =
  let fbs = sbToBlock <$> files
      ebs = feToBlock <$> filledEmpties
   in concat $ zipWith (++) fbs ebs

sbToBlock :: SizedBlock -> [Block]
sbToBlock (SizedBlock b size) = L.replicate (fromInteger size) b

feToBlock :: FilledEmpty -> [Block]
feToBlock (FilledEmpty size sbs) = concatMap sbToBlock (sbs ++ [SizedBlock Empty size])

moveFile :: Compressed -> SizedBlock -> Compressed
moveFile (Compressed files filledEmpties) sb@(SizedBlock (File id) size) =
  let midx = L.findIndex (\(FilledEmpty s _) -> s >= size) filledEmpties
   in case midx of
        Just idx ->
          if idx < fromInteger id
            then
              let (FilledEmpty s sbs) = filledEmpties L.!! idx
                  fe' = FilledEmpty (s - size) (sbs ++ [sb])
                  prefix = L.take idx filledEmpties
                  suffix = L.drop (idx + 1) filledEmpties
               in Compressed (toEmpty sb : files) (prefix ++ (fe' : suffix))
            else Compressed (sb : files) filledEmpties
        Nothing -> Compressed (sb : files) filledEmpties

part2 :: Text -> Integer
part2 input =
  let bs = parse' input
   in checksum $ compress' bs

day9 :: Text -> IO (String, String)
day9 input = do
  return (show $ part1 input, show $ part2 input)
