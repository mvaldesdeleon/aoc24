module Day4
  ( day4,
  )
where

import qualified Data.Text as T
import qualified Data.Vector as V
import Relude

data WordSearch = WordSearch
  { wsWidth :: Integer,
    wsHeight :: Integer,
    wsPuzzle :: V.Vector Char
  }
  deriving (Eq, Ord, Show)

data Dir = S | E | N | W | SE | SW | NE | NW
  deriving (Eq, Ord, Show)

toDeltas :: Dir -> [(Integer, Integer)]
toDeltas dir =
  case dir of
    S -> [(0, 0), (0, 1), (0, 2), (0, 3)]
    E -> [(0, 0), (1, 0), (2, 0), (3, 0)]
    N -> [(0, 0), (0, -1), (0, -2), (0, -3)]
    W -> [(0, 0), (-1, 0), (-2, 0), (-3, 0)]
    SE -> [(0, 0), (1, 1), (2, 2), (3, 3)]
    SW -> [(0, 0), (-1, 1), (-2, 2), (-3, 3)]
    NE -> [(0, 0), (1, -1), (2, -2), (3, -3)]
    NW -> [(0, 0), (-1, -1), (-2, -2), (-3, -3)]

charLookup :: WordSearch -> Integer -> (Integer, Integer) -> Maybe Char
charLookup (WordSearch width heigth puzzle) pos (dx, dy) =
  let x = (pos `mod` width) + dx
      y = (pos `div` width) + dy
      p = pos + dy * width + dx
   in if x >= 0 && x < width && y >= 0 && y < heigth
        then puzzle V.!? fromIntegral p
        else Nothing

isXmas' :: WordSearch -> Integer -> [(Integer, Integer)] -> Bool
isXmas' ws pos deltas =
  let word = mapMaybe (charLookup ws pos) deltas
   in word == "XMAS"

isX'mas' :: WordSearch -> Integer -> [(Integer, Integer)] -> Bool
isX'mas' ws pos deltas =
  let word = mapMaybe (charLookup ws pos) deltas
   in word == "MMASS"

isXmas :: WordSearch -> Integer -> Dir -> Bool
isXmas ws pos dir = isXmas' ws pos (toDeltas dir)

countXmas' :: WordSearch -> Integer -> Integer
countXmas' ws pos =
  toInteger
    . length
    . filter (== True)
    $ [ isXmas ws pos S,
        isXmas ws pos E,
        isXmas ws pos N,
        isXmas ws pos W,
        isXmas ws pos SE,
        isXmas ws pos SW,
        isXmas ws pos NE,
        isXmas ws pos NW
      ]

countX'mas' :: WordSearch -> Integer -> Integer
countX'mas' ws pos =
  toInteger
    . length
    . filter (== True)
    $ [ isX'mas' ws pos [(-1, -1), (-1, 1), (0, 0), (1, -1), (1, 1)],
        isX'mas' ws pos [(1, 1), (-1, 1), (0, 0), (1, -1), (-1, -1)],
        isX'mas' ws pos [(-1, -1), (1, -1), (0, 0), (-1, 1), (1, 1)],
        isX'mas' ws pos [(1, 1), (1, -1), (0, 0), (-1, 1), (-1, -1)]
      ]

countXmas :: WordSearch -> Integer
countXmas ws@(WordSearch width heigth puzzle) = sum $ countXmas' ws <$> [0 .. (width * heigth) - 1]

countX'mas :: WordSearch -> Integer
countX'mas ws@(WordSearch width heigth puzzle) = sum $ countX'mas' ws <$> [0 .. (width * heigth) - 1]

parseWordSearch :: Text -> WordSearch
parseWordSearch input =
  let rows = lines input
      height = length rows
      width = maybe 0 T.length (viaNonEmpty head rows)
   in WordSearch (toInteger width) (toInteger height) (V.fromList . T.unpack . T.filter (/= '\n') $ input)

part1 :: Text -> Integer
part1 = countXmas . parseWordSearch

part2 :: Text -> Integer
part2 = countX'mas . parseWordSearch

day4 :: Text -> IO (String, String)
day4 input = do
  return (show $ part1 input, show $ part2 input)
