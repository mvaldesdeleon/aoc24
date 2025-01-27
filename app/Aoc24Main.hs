{-# LANGUAGE TemplateHaskell #-}

module Aoc24Main where

import Control.Applicative ((<**>))
import Control.Monad (when)
import qualified Data.ByteString as BS
import Day1 (day1)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18)
import Day19 (day19)
import Day2 (day2)
import Day20 (day20)
import Day21 (day21)
import Day22 (day22)
import Day23 (day23)
import Day24 (day24)
import Day25 (day25)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Options.Applicative as OA
import Paths_aoc24 (getDataFileName)
import Relude
import Session (session)
import System.Directory (canonicalizePath, doesFileExist)
import TH (makeDayC, makeDayT, makeTOC)

makeDayT -- data Day = Day1 | Day2 | ...

dayId :: Day -> String
dayId day = show $ toInteger (fromEnum day) + 1

inputFilePath :: Day -> IO FilePath
inputFilePath day = getDataFileName ("inputs/day-" <> dayId day <> ".txt")

loadInput :: Day -> IO Text
loadInput day = do
  filepath <- inputFilePath day
  cached <- doesFileExist filepath
  if cached
    then do
      putStrLn "Using input file from cache"
      decodeUtf8 <$> readFileBS filepath
    else do
      putStrLn "Downloading input file"
      downloadAndSave day filepath

downloadAndSave :: Day -> FilePath -> IO Text
downloadAndSave day filepath = R.runReq R.defaultHttpConfig $ do
  response <- R.req R.GET (R.https "adventofcode.com" /: "2024" /: "day" /: toText (dayId day) /: "input") R.NoReqBody R.bsResponse (R.header "cookie" ("session=" <> session))
  let status = R.responseStatusCode response
  when (status /= 200) (error $ "Recived non-200 status code: " <> show status)
  let body = BS.init $ R.responseBody response
  writeFileBS filepath body
  return $ decodeUtf8 body

dayOption :: OA.Parser Day
dayOption =
  OA.subparser
    $ foldMap (\(c, t, d) -> pureCommand c t d) $(makeTOC)

pureCommand :: String -> String -> a -> OA.Mod OA.CommandFields a
pureCommand cmd desc val = OA.command cmd (OA.info (pure val) (OA.progDesc desc))

versionOption = OA.infoOption "1.0" (OA.short 'v' <> OA.long "version" <> OA.help "Show version" <> OA.hidden)

main :: IO ()
main = do
  (showPath, inputPath, day) <- OA.execParser (OA.info options (OA.fullDesc <> OA.progDesc "Runs the challenges for the given day" <> OA.header "Advent of Code 2024"))
  putStrLn $ "Running day " <> dayId day
  let manualInput = inputPath /= ""
  inputPath' <- canonicalizePath inputPath
  when showPath $ do
    path <- inputFilePath day
    putStrLn $ "Input file path: " <> path
  when manualInput $ do
    putStrLn $ "Override input file path: " <> inputPath'
  input <-
    if manualInput
      then decodeUtf8 <$> readFileBS inputPath'
      else loadInput day
  (part1, part2) <-
    $(makeDayC) day input
  putStrLn $ "Part 1: " <> part1
  putStrLn $ "Part 2: " <> part2
  where
    options = (,,) <$> pathOption <*> inputOption <*> dayOption <**> versionOption <**> OA.helper
    pathOption = OA.switch (OA.long "show-path" <> OA.help "Show the input file path")
    inputOption = OA.strOption (OA.long "input" <> OA.help "Override the input file path" <> OA.metavar "PATH" <> OA.value "")