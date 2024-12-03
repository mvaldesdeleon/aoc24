module Day3
  ( day3,
  )
where

import Data.Attoparsec.Text
import Relude hiding (Op)

data Op = Mul (Integer, Integer) | Do | Dont
  deriving
    (Show, Eq, Ord)

mul :: Op -> Integer
mul (Mul (x, y)) = x * y
mul _ = error "You goofed"

parseMul :: Parser Op
parseMul =
  let operands = (,) <$> ("mul" *> "(" *> decimal <* ",") <*> (decimal <* ")")
   in Mul <$> operands

parseDo :: Parser Op
parseDo = Do <$ "do()"

parseDont :: Parser Op
parseDont = Dont <$ "don't()"

parseOp :: Parser Op
parseOp = parseDo <|> parseDont <|> parseMul

parseOpCorrupted :: Parser Op
parseOpCorrupted = parseOp <|> (anyChar *> parseOpCorrupted)

parseMulCorrupted :: Parser Op
parseMulCorrupted = parseMul <|> (anyChar *> parseMulCorrupted)

parseOps :: Parser [Op]
parseOps = many1 parseOpCorrupted

parseMuls :: Parser [Op]
parseMuls = many1 parseMulCorrupted

part1 :: Text -> Integer
part1 input =
  let ops = case parseOnly parseMuls input of
        Right ops -> ops
        Left err -> error $ toText err
   in sum $ mul <$> ops

filterOps :: [Op] -> [Op]
filterOps = fst . foldl' processOp ([], True)
  where
    processOp (ops, enabled) op =
      case op of
        Mul _ -> if enabled then (op : ops, enabled) else (ops, enabled)
        Do -> (ops, True)
        Dont -> (ops, False)

part2 :: Text -> Integer
part2 input =
  let ops = case parseOnly parseOps input of
        Right ops -> ops
        Left err -> error $ toText err
   in sum $ mul <$> filterOps ops

day3 :: Text -> IO (String, String)
day3 input = do
  return (show $ part1 input, show $ part2 input)
