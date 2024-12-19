module Day19
  ( day19,
  )
where

import Control.Monad.ST
import Data.Array.Base (STUArray (STUArray))
import Data.Array.ST
import Data.Attoparsec.Text hiding (take)
import Data.Foldable (maximum)
import qualified Data.Set as S
import Relude

data Color = W | U | B | R | G
  deriving (Eq, Ord, Show)

type Towel = [Color]

type Design = [Color]

data Onsen = Onsen
  { oTowels :: [Towel],
    oDesigns :: [Design]
  }
  deriving (Eq, Ord, Show)

parseColors :: Parser [Color]
parseColors =
  let wubrg = "wubrg" :: [Char]
   in map toColor <$> many1 (satisfy (`elem` wubrg))
  where
    toColor 'w' = W
    toColor 'u' = U
    toColor 'b' = B
    toColor 'r' = R
    toColor 'g' = G

parseOnsen :: Parser Onsen
parseOnsen = do
  towels <- parseColors `sepBy1` ", "
  endOfLine
  endOfLine
  designs <- parseColors `sepBy1` endOfLine
  return $ Onsen towels designs

parseInput :: Text -> Onsen
parseInput input =
  case parseOnly parseOnsen input of
    Left err -> error $ toText err
    Right os -> os

isPossible :: S.Set Towel -> Int -> Design -> Bool
isPossible _ _ [] = True
isPossible towels maxLen design =
  let prefixes = makePrefix <$> [1 .. maxLen]
      validPrefixes = filter ((`S.member` towels) . fst) prefixes
   in any (isPossible towels maxLen . nextDesign) validPrefixes
  where
    makePrefix l = (take l design, l)
    nextDesign (_, l) = drop l design

countPossible :: S.Set Towel -> Int -> Design -> Int
countPossible towels maxLen design = runST $ do
  let designLen = length design
  memo <- newArray (0, designLen) (-1) :: ST s (STUArray s Int Int)
  go memo design
  where
    go :: STUArray s Int Int -> Design -> ST s Int
    go memo [] = return 1
    go memo design = do
      let designLen = length design
      count <- readArray memo designLen
      if count == (-1)
        then do
          let prefixes = makePrefix <$> reverse [1 .. min maxLen designLen]
              validPrefixes = filter ((`S.member` towels) . fst) prefixes

          counts <- mapM (go memo . nextDesign) validPrefixes
          let count' = sum counts
          writeArray memo designLen count'
          return count'
        else return count
      where
        makePrefix l = (take l design, l)
        nextDesign (_, l) = drop l design

part1 :: Text -> Integer
part1 input =
  let os = parseInput input
      towels = S.fromList (oTowels os)
      maxlen = maximum (length <$> oTowels os)
      possibles = isPossible towels maxlen <$> oDesigns os
   in toInteger . length $ filter id possibles

part2 :: Text -> Integer
part2 input =
  let os = parseInput input
      towels = S.fromList (oTowels os)
      maxlen = maximum (length <$> oTowels os)
      possibles = countPossible towels maxlen <$> oDesigns os
   in toInteger . sum $ possibles

day19 :: Text -> IO (String, String)
day19 input = do
  return (show $ part1 input, show $ part2 input)
