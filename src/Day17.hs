{-# LANGUAGE BinaryLiterals #-}

module Day17
  ( day17,
  )
where

import Control.Monad.Loops
import Data.Attoparsec.Text
import Data.Bits
import qualified Data.List as L
import qualified Data.Vector as V
import Relude

data Program = Program
  { pRA :: Integer,
    pRB :: Integer,
    pRC :: Integer,
    pProgram :: [Integer]
  }
  deriving (Eq, Ord, Show)

data Computer = Computer
  { cRA :: Integer,
    cRB :: Integer,
    cRC :: Integer,
    cIP :: Integer,
    cProgram :: V.Vector Integer,
    cOutput :: [Integer]
  }
  deriving (Eq, Ord, Show)

data Instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
  deriving (Eq, Ord, Show, Bounded, Enum)

parseProgram :: Parser Program
parseProgram = do
  ra <- "Register A: " *> decimal <* endOfLine
  rb <- "Register B: " *> decimal <* endOfLine
  rc <- "Register C: " *> decimal <* endOfLine
  endOfLine
  ps <- "Program: " *> decimal `sepBy1` ","
  return $ Program ra rb rc ps

initComputer :: Program -> Computer
initComputer (Program ra rb rc ps) = Computer ra rb rc 0 (V.fromList ps) []

initComputer' :: Computer -> Integer -> Computer
initComputer' c ra = c {cRA = ra}

runComputer :: State Computer [Integer]
runComputer = do
  whileM_ (not <$> halted) runInstruction
  gets (L.reverse . cOutput)

halted :: State Computer Bool
halted = do
  (Computer ra rb rc ip ps out) <- get
  return $ fromInteger ip >= V.length ps

runInstruction :: State Computer ()
runInstruction = do
  (Computer ra rb rc ip ps out) <- get
  let instruction = toEnum . fromInteger $ ps V.! fromInteger ip :: Instruction
      operand = ps V.! fromInteger (ip + 1)
  case instruction of
    ADV -> do
      let operand' = combo operand ra rb rc
          ra' = shift ra (fromInteger . negate $ operand')
      put (Computer ra' rb rc (ip + 2) ps out)
    BXL -> do
      let rb' = rb `xor` operand
      put (Computer ra rb' rc (ip + 2) ps out)
    BST -> do
      let operand' = combo operand ra rb rc
          rb' = operand' `mod` 8
      put (Computer ra rb' rc (ip + 2) ps out)
    JNZ ->
      if ra /= 0
        then do
          put (Computer ra rb rc operand ps out)
        else
          put (Computer ra rb rc (ip + 2) ps out)
    BXC -> do
      let rb' = rb `xor` rc
      put (Computer ra rb' rc (ip + 2) ps out)
    OUT -> do
      let operand' = combo operand ra rb rc
          result = operand' `mod` 8
      put (Computer ra rb rc (ip + 2) ps (result : out))
    BDV -> do
      let operand' = combo operand ra rb rc
          rb' = shift ra (fromInteger . negate $ operand')
      put (Computer ra rb' rc (ip + 2) ps out)
    CDV -> do
      let operand' = combo operand ra rb rc
          rc' = shift ra (fromInteger . negate $ operand')
      put (Computer ra rb rc' (ip + 2) ps out)
  where
    combo operand ra rb rc
      | operand == 4 = ra
      | operand == 5 = rb
      | operand == 6 = rc
      | operand == 7 = undefined
      | otherwise = operand

parseInput :: Text -> Program
parseInput input = case parseOnly parseProgram input of
  Left err -> error $ toText err
  Right p -> p

part1 :: Text -> [Char]
part1 input =
  let computer = initComputer . parseInput $ input
      out = evalState runComputer computer
   in L.intercalate "," $ show <$> out

tag :: (Show a) => String -> a -> a
tag label value = trace (label ++ show value) value

part2 :: Text -> Integer
part2 input =
  let program = parseInput input
      computer = initComputer program
      s1 = buildSeries 2 0 1
      s2 = buildSeries 5 1 6
      s3 = buildSeries 4 2 7
      s4 = buildSeries 7 3 4
      s5 = buildSeries 6 4 5
      twoSeries = minmin [s1, s2, s3, s4, s5]
      twoOneFourTwoSeries =
        filter (inSeries 2 . flip shift (-9))
          . filter (inSeries 1 . flip shift (-6))
          . filter (inSeries 4 . flip shift (-3))
          $ twoSeries
      top = shift 0b000001000011101101001 30 :: Int
      actualCandidates = (top .|.) <$> twoOneFourTwoSeries
      computers = initComputer' computer . toInteger <$> actualCandidates
      outs = evalState runComputer <$> computers
      target = pProgram program
      ra = snd . L.head . filter ((==) target . fst) $ zip outs actualCandidates
   in toInteger ra

minmin :: (Ord a) => [[a]] -> [a]
minmin ass =
  let ((m : _), idx) = L.minimumBy (compare `on` L.head . fst) (zip ass [0 ..])
   in m : minmin (pop idx <$> zip ass [0 ..])
  where
    pop :: Int -> ([a], Int) -> [a]
    pop idx (as, idx')
      | idx == idx' = L.tail as
      | otherwise = as

buildSeries :: Int -> Int -> Int -> [Int]
buildSeries s d e =
  let start = shift s (d + 3)
      middle = flip shift 3 <$> [0 .. shift 1 d - 1]
      inf = [0 ..] :: [Int]
      tails =
        if d == 0
          then [start + e]
          else (\m -> start .|. m .|. e) <$> middle
   in [shift i (d + 6) .|. t | i <- inf, t <- tails]

inSeries :: Int -> Int -> Bool
inSeries 0 i =
  or
    [ i .&. 0b1110000111 == 0b1000000101,
      i .&. 0b111000111 == 0b101000100,
      i .&. 0b11100111 == 0b11000111,
      i .&. 0b1110111 == 0b1110110,
      i .&. 0b111111 == 0b000001
    ]
inSeries 1 i =
  or
    [ i .&. 0b1110000111 == 0b1010000101,
      i .&. 0b111000111 == 0b100000100,
      i .&. 0b11100111 == 0b11100111,
      i .&. 0b1110111 == 0b1100110,
      i .&. 0b111111 == 0b001001,
      i .&. 0b11111 == 0b00000,
      i .&. 0b111 == 0b010
    ]
inSeries 2 i =
  or
    [ i .&. 0b1110000111 == 0b1100000101,
      i .&. 0b111000111 == 0b111000100,
      i .&. 0b11100111 == 0b10000111,
      i .&. 0b1110111 == 0b1010110,
      i .&. 0b111111 == 0b010001
    ]
inSeries 3 i =
  or
    [ i .&. 0b1110000111 == 0b1110000101,
      i .&. 0b111000111 == 0b110000100,
      i .&. 0b11100111 == 0b10100111,
      i .&. 0b1110111 == 0b1000110,
      i .&. 0b111111 == 0b011001,
      i .&. 0b11111 == 0b01000,
      i .&. 0b1111 == 0b0011
    ]
inSeries 4 i =
  or
    [ i .&. 0b1110000111 == 0b0000000101,
      i .&. 0b111000111 == 0b001000100,
      i .&. 0b11100111 == 0b01000111,
      i .&. 0b1110111 == 0b0110110,
      i .&. 0b111111 == 0b100001
    ]
inSeries 5 i =
  or
    [ i .&. 0b1110000111 == 0b0010000101,
      i .&. 0b111000111 == 0b000000100,
      i .&. 0b11100111 == 0b01100111,
      i .&. 0b1110111 == 0b0100110,
      i .&. 0b111111 == 0b101001,
      i .&. 0b11111 == 0b10000
    ]
inSeries 7 i =
  or
    [ i .&. 0b1110000111 == 0b0110000101,
      i .&. 0b111000111 == 0b010000100,
      i .&. 0b11100111 == 0b00100111,
      i .&. 0b1110111 == 0b0000110,
      i .&. 0b111111 == 0b111001,
      i .&. 0b11111 == 0b11000,
      i .&. 0b1111 == 0b1011
    ]

day17 :: Text -> IO (String, String)
day17 input = do
  return (part1 input, show $ part2 input)