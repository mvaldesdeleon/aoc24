{-# LANGUAGE TemplateHaskellQuotes #-}

module TH
  ( makeDayT,
    makeDayC,
    makeTOC,
  )
where

import Language.Haskell.TH
import Relude

challenges :: [String]
challenges =
  [ "Day 1: Historian Hysteria",
    "Day 2: Red-Nosed Reports",
    "Day 3: Mull It Over",
    "Day 4: Ceres Search",
    "Day 5: Print Queue",
    "Day 6: Guard Gallivant",
    "Day 7: Bridge Repair",
    "Day 8: Resonant Collinearity",
    "Day 9: Disk Fragmenter",
    "Day 10: Hoof It",
    "Day 11: Plutonian Pebbles",
    "Day 12: Garden Groups",
    "Day 13: Claw Contraption",
    "Day 14: Restroom Redoubt"
  ]

makeDayT :: Q [Dec]
makeDayT = do
  name <- newName "Day"
  cons <- traverse makeDayC [1 .. length challenges]
  return [DataD [] name [] Nothing cons [DerivClause Nothing [ConT ''Show, ConT ''Enum]]]
  where
    makeDayC i = do
      name <- newName ("Day" ++ show i)
      return $ NormalC name []

makeDayC :: Q Exp
makeDayC = do
  return $ LamCaseE (map makeDayM [1 .. length challenges])
  where
    makeDayM i = Match (ConP (mkName $ "Day" ++ show i) [] []) (NormalB (VarE (mkName $ "day" ++ show i))) []

makeTOC :: Q Exp
makeTOC = do
  return $ ListE (zipWith makeTuple [1 ..] challenges)
  where
    makeTuple i desc = TupE [Just (LitE (StringL $ "day" ++ show i)), Just (LitE (StringL desc)), Just (ConE (mkName $ "Day" ++ show i))]