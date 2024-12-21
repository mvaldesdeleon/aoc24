{-# LANGUAGE FlexibleContexts #-}

module Day21
  ( day21,
  )
where

import Control.Monad.ST
import Data.Array.ST
import qualified Data.List as L
import Data.STRef
import qualified Data.Text as T
import Relude

type CodeA = [Char]

type CodeB = [Char]

shortestPath :: CodeA -> Int
shortestPath code =
  let cpa = codePathsA code
      cpbs = iterate (concatMap codePathsB) cpa
   in L.minimum (L.length <$> (cpbs L.!! 2))

shortestPath' :: CodeA -> Int
shortestPath' code =
  let cpa = codePathsA code
   in L.minimum $ shortestPathB 25 <$> cpa

shortestPathB :: Int -> CodeB -> Int
shortestPathB n code = runST $ do
  memo <- newArray (0, 5 * 5 * 26) (-1) :: ST s (STUArray s Int16 Int)

  stShortestPath memo n code
  where
    toIdx n (f, t) = fromIntegral $ n * 25 + charIdx f * 5 + charIdx t
    charIdx c = fromMaybe undefined (c `L.elemIndex` ['A', '<', '^', 'v', '>'])

    stShortestPath :: STUArray s Int16 Int -> Int -> CodeB -> ST s Int
    stShortestPath memo n code = do
      let code' = 'A' : code
      let pairs = zip code' (drop 1 code')

      sps <- mapM (stShortestPath' memo n) pairs

      return $ sum sps

    stShortestPath' :: STUArray s Int16 Int -> Int -> (Char, Char) -> ST s Int
    stShortestPath' memo n (f, t) = do
      let idx = toIdx n (f, t)
      m <- readArray memo idx
      if m == (-1)
        then
          if n == 0
            then do
              let m = 1
              writeArray memo idx m
              return $ m
            else do
              let codes = (++ "A") <$> charPathsB f t
              sps <- mapM (stShortestPath memo (n - 1)) codes
              let m = L.minimum sps
              writeArray memo idx m
              return $ m
        else return m

codePathsA :: CodeA -> [CodeB]
codePathsA code =
  let code' = 'A' : code
      pairPaths = map (++ "A") <$> zipWith charPathsA code' (drop 1 code')
   in nwayMerge pairPaths

nwayMerge :: [[CodeB]] -> [CodeB]
nwayMerge [] = [[]]
nwayMerge (a : as) = (++) <$> a <*> nwayMerge as

codePathsB :: CodeB -> [CodeB]
codePathsB code =
  let code' = 'A' : code
      pairPaths = map (++ "A") <$> zipWith charPathsB code' (drop 1 code')
   in nwayMerge pairPaths

toChar' :: (Int, Int) -> (Int, Int) -> Char
toChar' (fx, fy) (tx, ty)
  | fx + 1 == tx = '>'
  | fx - 1 == tx = '<'
  | fy + 1 == ty = 'v'
  | fy - 1 == ty = '^'

charPathsA :: Char -> Char -> [CodeB]
charPathsA from to = map toChar . filter ((0, 3) `notElem`) $ rawPaths (toCoords from) (toCoords to)
  where
    toCoords '7' = (0, 0)
    toCoords '8' = (1, 0)
    toCoords '9' = (2, 0)
    toCoords '4' = (0, 1)
    toCoords '5' = (1, 1)
    toCoords '6' = (2, 1)
    toCoords '1' = (0, 2)
    toCoords '2' = (1, 2)
    toCoords '3' = (2, 2)
    toCoords '0' = (1, 3)
    toCoords 'A' = (2, 3)

    toChar :: [(Int, Int)] -> [Char]
    toChar cs =
      let cs' = toCoords from : cs
       in zipWith toChar' cs' (drop 1 cs')

charPathsB :: Char -> Char -> [CodeB]
charPathsB from to = map toChar . filter ((0, 0) `notElem`) $ rawPaths (toCoords from) (toCoords to)
  where
    toCoords '^' = (1, 0)
    toCoords 'A' = (2, 0)
    toCoords '<' = (0, 1)
    toCoords 'v' = (1, 1)
    toCoords '>' = (2, 1)

    toChar :: [(Int, Int)] -> [Char]
    toChar cs =
      let cs' = toCoords from : cs
       in zipWith toChar' cs' (drop 1 cs')

rawPaths :: (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
rawPaths (fx, fy) (tx, ty)
  | fx == tx && fy == ty = [[]]
  | fx == tx =
      let dy = if ty > fy then 1 else -1
       in ((fx, fy + dy) :) <$> rawPaths (fx, fy + dy) (tx, ty)
  | fy == ty =
      let dx = if tx > fx then 1 else -1
       in ((fx + dx, fy) :) <$> rawPaths (fx + dx, fy) (tx, ty)
  | otherwise =
      let dx = if tx > fx then 1 else -1
          dy = if ty > fy then 1 else -1
          pathsA = ((fx + dx, fy) :) <$> rawPaths (fx + dx, fy) (tx, ty)
          pathsB = ((fx, fy + dy) :) <$> rawPaths (fx, fy + dy) (tx, ty)
       in pathsA ++ pathsB

part1 :: Text -> Int
part1 input =
  let codes = T.unpack <$> lines input
      nunCodes = mapMaybe (readMaybe . take 3) codes :: [Int]
      paths = shortestPath <$> codes
   in sum $ zipWith (*) nunCodes paths

part2 :: Text -> Int
part2 input =
  let codes = T.unpack <$> lines input
      nunCodes = mapMaybe (readMaybe . take 3) codes :: [Int]
      paths = shortestPath' <$> codes
   in sum $ zipWith (*) nunCodes paths

day21 :: Text -> IO (String, String)
day21 input = do
  return (show $ part1 input, show $ part2 input)