{-# LANGUAGE FlexibleContexts #-}

module Day12
  ( day12,
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char
import qualified Data.List as L
import Data.STRef
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data GardenMap = GardenMap
  { gmWidth :: Integer,
    gmHeight :: Integer,
    gmPlots :: V.Vector Char
  }
  deriving (Eq, Ord, Show)

parseGardenMap :: Text -> GardenMap
parseGardenMap input =
  let rows = lines input
      height = length rows
      width = maybe 0 T.length (viaNonEmpty head rows)
   in GardenMap (toInteger width) (toInteger height) (V.fromList . T.unpack . T.filter (/= '\n') $ input)

fencePrice :: GardenMap -> Integer
fencePrice (GardenMap width height plots) = runST $ do
  plotsArr <- newListArray (0, V.length plots - 1) (V.toList plots) :: ST s (STUArray s Int Char)
  foldM (regionPrice width height plotsArr) 0 [0 .. V.length plots - 1]

regionPrice :: Integer -> Integer -> STUArray s Int Char -> Integer -> Int -> ST s Integer
regionPrice width height plotsArr price idx = do
  plot <- readArray plotsArr idx
  if isLower plot
    then return price
    else do
      (area, perimeter) <- floodFill width height plotsArr plot (toCoords idx)
      return (price + area * perimeter)
  where
    toCoords idx = (toInteger idx `mod` width, toInteger idx `div` width)

floodFill :: Integer -> Integer -> STUArray s Int Char -> Char -> (Integer, Integer) -> ST s (Integer, Integer)
floodFill width height plotsArr plot (x, y) = go (x, y) 0 0
  where
    go (x, y) area perim =
      if x >= 0 && x < width && y >= 0 && y < height
        then do
          plot' <- readArray plotsArr (toIndex (x, y))
          if plot == plot'
            then do
              writeArray plotsArr (toIndex (x, y)) (toLower plot)
              let (a0, p0) = (area + 1, perim)
              (a1, p1) <- go (x + 1, y) a0 p0
              (a2, p2) <- go (x, y + 1) a1 p1
              (a3, p3) <- go (x - 1, y) a2 p2
              go (x, y - 1) a3 p3
            else
              if plot' == toLower plot
                then
                  return (area, perim)
                else
                  return (area, perim + 1)
        else
          return (area, perim + 1)
    toIndex :: (Integer, Integer) -> Int
    toIndex (x, y) = fromInteger $ y * width + x

part1 :: Text -> Integer
part1 = fencePrice . parseGardenMap

data Facing = Next | Prev
  deriving (Eq, Ord, Show)

data Wall = Vertical (Integer, Integer) Facing | Horizontal (Integer, Integer) Facing
  deriving (Eq, Ord, Show)

data Dir = N | S | E | W | X
  deriving (Eq, Ord, Show)

data WalledRegion = WalledRegion Integer [Wall]
  deriving (Eq, Ord, Show)

countSides :: [Wall] -> Integer
countSides walls = L.genericLength . L.nub $ buildSide walls <$> walls

buildSide :: [Wall] -> Wall -> [Wall]
buildSide ws w = L.sort $ [w] ++ goP w ws ++ goN w ws
  where
    goP w ws =
      case w of
        Vertical (x, y) fc ->
          let p = Vertical (x, y - 1) fc
           in if p `L.elem` ws then p : goP p ws else []
        Horizontal (x, y) fc ->
          let p = Horizontal (x - 1, y) fc
           in if p `L.elem` ws then p : goP p ws else []
    goN w ws =
      case w of
        Vertical (x, y) fc ->
          let p = Vertical (x, y + 1) fc
           in if p `L.elem` ws then p : goN p ws else []
        Horizontal (x, y) fc ->
          let p = Horizontal (x + 1, y) fc
           in if p `L.elem` ws then p : goN p ws else []

discountedPrice :: WalledRegion -> Integer
discountedPrice (WalledRegion area walls) =
  let sides = countSides walls
   in area * sides

walledRegions :: GardenMap -> [WalledRegion]
walledRegions (GardenMap width height plots) = runST $ do
  plotsArr <- newListArray (0, V.length plots - 1) (V.toList plots) :: ST s (STUArray s Int Char)
  foldM (regionWalls width height plotsArr) [] [0 .. V.length plots - 1]

regionWalls :: Integer -> Integer -> STUArray s Int Char -> [WalledRegion] -> Int -> ST s [WalledRegion]
regionWalls width height plotsArr wrs idx = do
  plot <- readArray plotsArr idx
  if isLower plot
    then return wrs
    else do
      (area, walls) <- floodFill' width height plotsArr plot (toCoords idx)
      return (WalledRegion area walls : wrs)
  where
    toCoords idx = (toInteger idx `mod` width, toInteger idx `div` width)

floodFill' :: Integer -> Integer -> STUArray s Int Char -> Char -> (Integer, Integer) -> ST s (Integer, [Wall])
floodFill' width height plotsArr plot (x, y) = go (x, y) X 0 []
  where
    go (x, y) dir area walls
      | x < 0 = return (area, Vertical (x + 1, y) Prev : walls)
      | x >= width = return (area, Vertical (x, y) Next : walls)
      | y < 0 = return (area, Horizontal (x, y + 1) Prev : walls)
      | y >= height = return (area, Horizontal (x, y) Next : walls)
      | otherwise = do
          plot' <- readArray plotsArr (toIndex (x, y))
          if plot == plot'
            then do
              writeArray plotsArr (toIndex (x, y)) (toLower plot)
              let (a0, p0) = (area + 1, walls)
              (a1, p1) <- go (x + 1, y) E a0 p0
              (a2, p2) <- go (x, y + 1) S a1 p1
              (a3, p3) <- go (x - 1, y) W a2 p2
              go (x, y - 1) N a3 p3
            else
              if plot' == toLower plot
                then
                  return (area, walls)
                else case dir of
                  N -> return (area, Horizontal (x, y + 1) Prev : walls)
                  E -> return (area, Vertical (x, y) Next : walls)
                  S -> return (area, Horizontal (x, y) Next : walls)
                  W -> return (area, Vertical (x + 1, y) Prev : walls)
    toIndex :: (Integer, Integer) -> Int
    toIndex (x, y) = fromInteger $ y * width + x

part2 :: Text -> Integer
part2 = sum . map discountedPrice . walledRegions . parseGardenMap

day12 :: Text -> IO (String, String)
day12 input = do
  return (show $ part1 input, show $ part2 input)
