module Day14
  ( day14,
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Attoparsec.Text
import qualified Data.List as L
import Data.NumInstances.Tuple
import qualified Data.Text as T
import Relude

bWidth = 101 -- 11

bHeight = 103 -- 7

data Robot = Robot
  { rPosition :: (Integer, Integer),
    rVelocity :: (Integer, Integer)
  }
  deriving (Eq, Ord, Show)

parseCoords :: Parser (Integer, Integer)
parseCoords = (,) <$> signed decimal <* "," <*> signed decimal

parseRobot :: Parser Robot
parseRobot = Robot <$> ("p=" *> parseCoords) <*> (" v=" *> parseCoords)

moveRobot :: Integer -> Robot -> Robot
moveRobot s (Robot p v) = Robot (wrapAround (p + v * fromInteger s)) v

wrapAround :: (Integer, Integer) -> (Integer, Integer)
wrapAround (x, y) = (x `mod` bWidth, y `mod` bHeight)

parseInput :: Text -> [Robot]
parseInput input = case parseOnly (parseRobot `sepBy1` endOfLine) input of
  Left err -> error $ toText err
  Right rs -> rs

groupByQuadrant :: [Robot] -> ([Robot], [Robot], [Robot], [Robot])
groupByQuadrant rs =
  let halfW = bWidth `div` 2
      halfH = bHeight `div` 2
      q1 = filter (inRect 0 0 halfW halfH) rs
      q2 = filter (inRect (halfW + 1) 0 bWidth halfH) rs
      q3 = filter (inRect 0 (halfH + 1) halfW bHeight) rs
      q4 = filter (inRect (halfW + 1) (halfH + 1) bWidth bHeight) rs
   in (q1, q2, q3, q4)
  where
    inRect x1 y1 x2 y2 (Robot (x, y) _) = x1 <= x && x < x2 && y1 <= y && y < y2

part1 :: Text -> Integer
part1 input =
  let rs = parseInput input
      rs' = moveRobot 100 <$> rs
      (q1, q2, q3, q4) = groupByQuadrant rs'
   in L.genericLength q1 * L.genericLength q2 * L.genericLength q3 * L.genericLength q4

display :: [Robot] -> IO ()
display = print . render
  where
    render rs = runST $ do
      bathroom <- newArray (0, fromInteger $ bWidth * bHeight - 1) '.' :: ST s (STUArray s Int Char)
      mapM_ (\(Robot (x, y) _) -> writeArray bathroom (fromInteger $ y * bWidth + x) '#') rs --- FIXME
      T.pack <$> getElems bathroom
    print input = mapM_ (putStrLn . T.unpack) (T.chunksOf (fromInteger bWidth) input)

display' :: ([Robot], Integer) -> IO ()
display' (rs, t) = do
  putStrLn $ "t=" ++ show t
  display rs

-- isInteresting :: ([Robot], Integer) -> Bool
-- isInteresting (rs, _) =
--   let (q1, q2, q3, q4) = groupByQuadrant rs
--    in L.genericLength (unique q1) == L.genericLength (unique q2) && L.genericLength (unique q3) == L.genericLength (unique q4)
--   where
--     unique = L.nubBy ((==) `on` rPosition)

isInteresting :: ([Robot], Integer) -> Bool
isInteresting (rs, _) =
  let rs' = L.sortBy (comparing (rPosition)) rs
   in (>= 3) . fst $ foldl' isNext (0, (0, 0)) rs'
  where
    isNext (s, (px, py)) (Robot (x, y) _) =
      if px == x && abs (y - py) <= 1
        then (s + 1, (x, y))
        else (1, (x, y))

day14 :: Text -> IO (String, String)
day14 input = do
  let rs = parseInput input
      ts = [88, 191 ..]
      rss = (\i -> moveRobot i <$> rs) <$> ts
  mapM_ display' $ L.take 1000 $ zip rss ts
  return (show $ part1 input, "N/A")
