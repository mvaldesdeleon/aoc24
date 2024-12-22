{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

module Day22
  ( day22,
  )
where

import Control.Monad.ST
import Data.Array.ST
import Data.Bits
import qualified Data.List as L
import Data.STRef
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Relude

type SecretNumber = Int

nextSN :: SecretNumber -> SecretNumber
nextSN sn =
  let sn' = (shift sn 6 `xor` sn) .&. 0b111111111111111111111111
      sn'' = (shift sn' (-5) `xor` sn') .&. 0b111111111111111111111111
      sn''' = (shift sn'' 11 `xor` sn'') .&. 0b111111111111111111111111
   in sn'''

parseInput :: Text -> [SecretNumber]
parseInput input = unsafeParseInteger <$> lines input
  where
    unsafeParseInteger str =
      case TR.decimal str of
        Right (i, _) -> i
        Left _ -> undefined

part1 :: Text -> Integer
part1 input =
  let sns0 = parseInput input
      sns2000 = fromIntegral . (L.!! 2000) . iterate nextSN <$> sns0 :: [Integer]
   in sum sns2000

part2 :: Text -> Integer
part2 input =
  let sns0 = parseInput input
   in maxBananas sns0

maxBananas :: [SecretNumber] -> Integer
maxBananas sns = runST $ do
  let buyers = fromIntegral $ length sns
  buyerSeq <- newArray (0, 160000 * buyers) (-1) :: ST s (STUArray s Int Int)
  mapM_ (stBuyerSeq buyerSeq) (zip sns [0 ..])

  bananas <- mapM (stCountBananas buyerSeq) [0 .. 160000 - 1]
  return $ toInteger $ L.maximum bananas
  where
    seqId a b c d =
      let a' = a + 9
          b' = b + 9
          c' = c + 9
          d' = d + 9
       in a' * 8000 + b' * 400 + c' * 20 + d'

    seqAndPrice a b c d price = (seqId a b c d, price)

    stBuyerSeq :: STUArray s Int Int -> (SecretNumber, Int) -> ST s ()
    stBuyerSeq buyerSeq (sn, buyerId) = do
      let prices = take 2000 $ fromIntegral . (`mod` 10) <$> iterate nextSN sn :: [Int]
          changes = zipWith (-) (drop 1 prices) prices
          seqs = L.zipWith5 seqAndPrice changes (drop 1 changes) (drop 2 changes) (drop 3 changes) (drop 4 prices)
      forM_ seqs $ \(seqId, price) -> do
        prev <- readArray buyerSeq (buyerId * 160000 + fromIntegral seqId)
        when (prev == -1) $ writeArray buyerSeq (buyerId * 160000 + fromIntegral seqId) price

    stCountBananas :: STUArray s Int Int -> Int -> ST s Int
    stCountBananas buyerSeq seqId = do
      let buyers = fromIntegral $ length sns
      bananas <- forM [0 .. buyers - 1] $ \buyerId -> do
        price <- readArray buyerSeq (buyerId * 160000 + fromIntegral seqId)
        return $ if price == -1 then 0 else price
      return $ sum bananas

day22 :: Text -> IO (String, String)
day22 input = do
  return (show $ part1 input, show $ part2 input)