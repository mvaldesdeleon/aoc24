module Day5
  ( day5,
  )
where

import Data.Attoparsec.Text
import Data.List (intersect, sortBy, (!!))
import qualified Data.Map.Strict as M
import Relude

newtype Rule = Rule (Integer, Integer)
  deriving (Eq, Show, Ord)

newtype Update = Update [Integer]
  deriving (Eq, Show, Ord)

data UpdateData = UpdateData
  { udRules :: [Rule],
    udUpdates :: [Update]
  }
  deriving (Eq, Show, Ord)

indexRules :: [Rule] -> M.Map Integer [Integer]
indexRules = foldr insertRule M.empty
  where
    insertRule (Rule (a, b)) = M.insertWith (++) a [b]

isValid :: M.Map Integer [Integer] -> Update -> Bool
isValid map (Update ps) = validate $ reverse ps
  where
    validate [] = True
    validate (p : ps) =
      let rulePages = M.findWithDefault [] p map
          is = intersect ps rulePages
       in null is && validate ps

parseRule :: Parser Rule
parseRule = Rule <$> ((,) <$> decimal <* "|" <*> decimal)

parseUpdate :: Parser Update
parseUpdate = Update <$> decimal `sepBy1` ","

parseUpdateData :: Parser UpdateData
parseUpdateData =
  let rules = parseRule `sepBy1` endOfLine
      updates = parseUpdate `sepBy1` endOfLine
   in UpdateData <$> rules <* count 2 endOfLine <*> updates

parseInput :: Text -> UpdateData
parseInput input =
  case parseOnly parseUpdateData input of
    Left err -> error $ toText err
    Right ud -> ud

middle :: Update -> Integer
middle (Update ps) =
  let size = length ps
   in ps !! (size `div` 2)

part1 :: Text -> Integer
part1 input =
  let (UpdateData rules updates) = parseInput input
      map = indexRules rules
      validUpdates = filter (isValid map) updates
   in sum $ middle <$> validUpdates

comparePages :: M.Map Integer [Integer] -> Integer -> Integer -> Ordering
comparePages map a b =
  let rulePagesA = M.findWithDefault [] a map
      rulePagesB = M.findWithDefault [] b map
   in if b `elem` rulePagesA
        then
          LT
        else
          if a `elem` rulePagesB
            then
              GT
            else
              EQ

fixUpdate :: M.Map Integer [Integer] -> Update -> Update
fixUpdate map (Update ps) = Update $ sortBy (comparePages map) ps

part2 :: Text -> Integer
part2 input =
  let (UpdateData rules updates) = parseInput input
      map = indexRules rules
      invalidUpdates = filter (not . isValid map) updates
      fixedUpdates = fixUpdate map <$> invalidUpdates
   in sum $ middle <$> fixedUpdates

day5 :: Text -> IO (String, String)
day5 input = do
  return (show $ part1 input, show $ part2 input)
