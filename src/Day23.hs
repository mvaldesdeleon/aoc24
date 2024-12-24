module Day23
  ( day23,
  )
where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Relude

type Computer = [Char]

type Connection = (Computer, Computer)

parseInput :: Text -> [Connection]
parseInput input = toConnection <$> lines input
  where
    toConnection conn =
      let compA = T.unpack $ T.take 2 conn
          compB = T.unpack $ T.takeEnd 2 conn
       in if compA <= compB then (compA, compB) else (compB, compA)

exists :: S.Set Connection -> Connection -> Bool
exists adjacency (compA, compB) = S.member (if compA <= compB then (compA, compB) else (compB, compA)) adjacency

buildNeighbors :: [Connection] -> M.Map Computer [Computer]
buildNeighbors = foldr addConnection M.empty
  where
    addConnection (compA, compB) map =
      let map' = M.insertWith (++) compA [compB] map
          map'' = M.insertWith (++) compB [compA] map'
       in map''

groups :: S.Set Connection -> M.Map Computer [Computer] -> Computer -> [[Computer]]
groups adjacency neighbors comp =
  let ns = fromMaybe [] $ neighbors M.!? comp
      conns = filter (exists adjacency) . filter (uncurry (<)) $ (,) <$> ns <*> ns :: [Connection]
   in addComputer <$> conns
  where
    addComputer (compA, compB) = L.sort [comp, compA, compB]

grow' :: S.Set Connection -> [[Computer]] -> [[Computer]]
grow' adjacency groups =
  let ns = filter close $ (,) <$> groups <*> groups
   in L.nub $ mapMaybe tryJoin ns
  where
    diff (grA, grB) = (grA L.\\ grB) ++ (grB L.\\ grA)
    intersect (grA, grB) = grA `L.intersect` grB
    close groupPair = L.length (diff groupPair) == 2 && uncurry (<) groupPair
    tryJoin :: ([Computer], [Computer]) -> Maybe [Computer]
    tryJoin groupPair =
      let [compA, compB] = diff groupPair
       in if exists adjacency (compA, compB)
            then Just $ L.sort $ [compA, compB] ++ intersect groupPair
            else Nothing

maxGrow' :: S.Set Connection -> [[Computer]] -> [[Computer]]
maxGrow' adjacency groups =
  let next = grow' adjacency groups
   in if null next then groups else maxGrow' adjacency next

grow :: S.Set Connection -> M.Map Computer [Computer] -> Computer -> [Computer] -> [[Computer]]
grow adjacency neighbors comp group =
  let ns = fromMaybe [] $ neighbors M.!? comp
   in mapMaybe tryGrow (ns L.\\ group)
  where
    tryGrow :: Computer -> Maybe [Computer]
    tryGrow comp =
      if all (exists adjacency . (,) comp) group
        then Just $ L.sort (comp : group)
        else Nothing

part1 :: Text -> Integer
part1 input =
  let conns = parseInput input
      adjacency = S.fromList conns
      neighbors = buildNeighbors conns
      compsH = [['t', c] | c <- ['a' .. 'z']] :: [Computer]
   in L.genericLength . L.nub $ concatMap (groups adjacency neighbors) compsH

part2 :: Text -> [Char]
part2 input =
  let conns = parseInput input
      adjacency = S.fromList conns
      neighbors = buildNeighbors conns
      compsH = [[a, b] | a <- ['a' .. 'z'], b <- ['a' .. 'z']] :: [Computer]
      gs = groups adjacency neighbors <$> compsH
      gs' = maxGrow' adjacency <$> gs
   in L.intercalate "," $ L.maximumBy (comparing L.length) (concat gs')

day23 :: Text -> IO (String, String)
day23 input = do
  return (show $ part1 input, show $ part2 input)
