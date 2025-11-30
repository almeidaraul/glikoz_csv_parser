{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- BL.readFile filename
            let allLines = BLC.lines content
                -- Skip lines until we find the first "entry" line
                linesFromFirstEntry = dropWhile (not . startsWithEntry) allLines
                -- Group lines starting with "entry"
                groups = groupByEntry linesFromFirstEntry
            mapM_ printGroup groups
        _ -> do
            hPutStrLn stderr "Usage: glikoz-csv-parser <filename.csv>"
            exitFailure

-- Check if a line starts with "entry" (case-insensitive)
startsWithEntry :: BL.ByteString -> Bool
startsWithEntry line = "\"entry\"" `isPrefixOf` BLC.unpack line

-- Group lines: each group starts with an "entry" line and includes all following lines
-- until the next "entry" line
groupByEntry :: [BL.ByteString] -> [[BL.ByteString]]
groupByEntry [] = []
groupByEntry (l:ls) = 
    let (group, rest) = collectGroup ls
    in (l : group) : groupByEntry rest
  where
    collectGroup [] = ([], [])
    collectGroup (x:xs)
        | startsWithEntry x = ([], x:xs)
        | otherwise = let (g, r) = collectGroup xs in (x:g, r)

-- Print a group of lines
printGroup :: [BL.ByteString] -> IO ()
printGroup group = do
    mapM_ BLC.putStrLn group
    putStrLn ""  -- Empty line between groups
