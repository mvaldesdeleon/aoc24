module Day24
  ( day24,
  )
where

import Control.Monad.Loops
import Control.Monad.ST
import Data.Array.ST
import Data.Attoparsec.Text
import Data.Bits
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Relude

data CableState = Unset | One | Zero
  deriving (Eq, Ord, Show)

cableStateToInt Unset = -1
cableStateToInt One = 1
cableStateToInt Zero = 0

type CableName = [Char]

data Cable = Cable CableName CableState
  deriving (Eq, Ord, Show)

data GateType = AND | OR | XOR
  deriving (Eq, Ord, Show)

data Gate = Gate
  { gType :: GateType,
    gInputA :: CableName,
    gInputB :: CableName,
    gOutput :: CableName
  }
  deriving (Eq, Ord, Show)

gateCables :: Gate -> [CableName]
gateCables (Gate _ inputA inputB output) = [inputA, inputB, output]

runGate :: GateType -> Int8 -> Int8 -> Int8
runGate AND inA inB = inA .&. inB
runGate OR inA inB = inA .|. inB
runGate XOR inA inB = inA `xor` inB

data Device = Device
  { dCables :: [Cable],
    dGates :: [Gate]
  }
  deriving (Eq, Ord, Show)

parseCable :: Parser Cable
parseCable = do
  name <- count 3 anyChar
  ": "
  state <- digit
  return $ case state of
    '1' -> Cable name One
    '0' -> Cable name Zero

parseGateType :: Parser GateType
parseGateType = (AND <$ "AND") <|> (OR <$ "OR") <|> (XOR <$ "XOR")

parseGate :: Parser Gate
parseGate = do
  inputA <- count 3 anyChar
  " "
  gateType <- parseGateType
  " "
  inputB <- count 3 anyChar
  " -> "
  output <- count 3 anyChar
  return $ Gate gateType inputA inputB output

parseDevice :: Parser Device
parseDevice = do
  cables <- parseCable `sepBy1` endOfLine
  endOfLine
  endOfLine
  gates <- parseGate `sepBy1` endOfLine
  let extraCables = concatMap (map toCable . filterInputs . gateCables) gates
  return $ Device (cables ++ extraCables) gates
  where
    filterInputs = filter ((`L.notElem` ("xy" :: [Char])) . L.head)
    toCable name = Cable name Unset

runDevice :: Device -> Integer
runDevice (Device cables gates) =
  let cableIndeces = foldr addCable M.empty (zip cables ([0 ..] :: [Int]))
      cableGates = foldr (addGate cableIndeces) M.empty (zip gates ([0 ..] :: [Int]))
      gatesVec = V.fromList gates
   in runST $ do
        cableStates <- newListArray (0, length cables - 1) (cableToInt <$> cables) :: ST s (STUArray s Int Int8)
        gateStates <- newArray (0, length gates - 1) (0) :: ST s (STUArray s Int Int8)
        cableStack <- newArray (0, length cables - 1) (-1) :: ST s (STUArray s Int Int)
        stackSize <- newSTRef 0

        stInit cableStates cableStack stackSize

        whileM_ (not <$> stDone stackSize) $ do
          next <- stFindNext cableStack stackSize
          let gs = fromMaybe [] $ M.lookup next cableGates

          forM_ gs $ \idx -> do
            state <- readArray gateStates idx
            writeArray gateStates idx (state + 1)
            when (state == 1) $ do
              let (Gate gateType inputA inputB output) = gatesVec V.! idx
              inA <- readArray cableStates (cableIndeces M.! inputA)
              inB <- readArray cableStates (cableIndeces M.! inputB)
              let out = runGate gateType inA inB
              writeArray cableStates (cableIndeces M.! output) out
              stAddCable cableStack stackSize (cableIndeces M.! output)

        let zs = [['z', a, b] | a <- ['0' .. '4'], b <- ['0' .. '9']] :: [CableName]
        bits <- forM zs $ \z -> do
          case M.lookup z cableIndeces of
            Just idx -> readArray cableStates idx
            Nothing -> return 0
        let result = foldl' setBit zeroBits $ map snd . filter ((==) 1 . fst) $ zip bits [0 ..] :: Integer
        return result -- get the result from the z cables
  where
    cableToInt (Cable _ state) = cableStateToInt state
    addCable (Cable name _, idx) = M.insert name idx
    addGate cableIndeces (Gate _ inputA inputB _, idx) map =
      let map' = M.insertWith (++) (cableIndeces M.! inputA) [idx] map
          map'' = M.insertWith (++) (cableIndeces M.! inputB) [idx] map'
       in map''

    stInit :: STUArray s Int Int8 -> STUArray s Int Int -> STRef s Int -> ST s ()
    stInit cableStates cableStack stackSize = do
      forM_ [0 .. length cables - 1] $ \idx -> do
        state <- readArray cableStates idx
        when (state /= -1) $ stAddCable cableStack stackSize idx

    stAddCable :: STUArray s Int Int -> STRef s Int -> Int -> ST s ()
    stAddCable cableStack stackSize idx = do
      last <- readSTRef stackSize
      writeArray cableStack last idx
      modifySTRef' stackSize succ

    stDone :: STRef s Int -> ST s Bool
    stDone stackSize = do
      last <- readSTRef stackSize
      return $ last == 0

    stFindNext :: STUArray s Int Int -> STRef s Int -> ST s Int
    stFindNext cableStack stackSize = do
      last <- readSTRef stackSize
      modifySTRef' stackSize pred
      readArray cableStack (last - 1)

parseInput :: Text -> Device
parseInput input =
  case parseOnly parseDevice input of
    Left err -> error $ toText err
    Right dev -> dev

part1 :: Text -> Integer
part1 input =
  let dev = parseInput input
   in runDevice dev

findNextDefect :: Device -> [CableName]
findNextDefect (Device cables gates) =
  let gatesMap = foldr addGate M.empty gates
   in snd $ foldl' (checkCircuit gatesMap) ("gjk", []) [1 .. 44]
  where
    addGate (Gate gateType inputA inputB output) = M.insert (gateType, inputA, inputB) output

    checkCircuit :: Map (GateType, CableName, CableName) CableName -> (CableName, [CableName]) -> Int -> (CableName, [CableName])
    checkCircuit gatesMap (carry, badCables) bit =
      let res = checkCircuit' gatesMap carry bit
       in case res of
            Just nextCarry -> (nextCarry, badCables)
            Nothing -> (carry, makeName 'x' bit : badCables)

    checkCircuit' :: Map (GateType, CableName, CableName) CableName -> CableName -> Int -> Maybe CableName
    checkCircuit' gatesMap carry bit = do
      let cableX = makeName 'x' bit
          cableY = makeName 'y' bit
          cableZ = makeName 'z' bit
      x0 <- lookup (XOR, cableX, cableY) gatesMap
      a0 <- lookup (AND, cableX, cableY) gatesMap

      x1 <- lookup (XOR, x0, carry) gatesMap
      a1 <- lookup (AND, x0, carry) gatesMap
      o1 <- lookup (OR, a0, a1) gatesMap
      if x1 == cableZ then Just o1 else Nothing

    lookup :: (GateType, CableName, CableName) -> Map (GateType, CableName, CableName) CableName -> Maybe CableName
    lookup (gateType, inputA, inputB) map = M.lookup (gateType, inputA, inputB) map <|> M.lookup (gateType, inputB, inputA) map

makeName :: Char -> Int -> CableName
makeName c bit = c : fixedWith bit
  where
    fixedWith bit =
      let b = show bit
       in if length b == 2 then b else '0' : b

part2 :: Text -> Maybe CableName
part2 input =
  let dev = parseInput input
   in viaNonEmpty head (reverse $ findNextDefect dev)

day24 :: Text -> IO (String, String)
day24 input = do
  return (show $ part1 input, show $ part2 input)
