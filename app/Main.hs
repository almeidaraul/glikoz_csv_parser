{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import DiaguardParser

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
            -- Print CSV headers
            putStrLn "date,glucose,fast_insulin,basal_insulin,carbs"
            mapM_ printGroup groups
        _ -> do
            hPutStrLn stderr "Usage: glikoz-csv-parser <filename.csv>"
            exitFailure

-- Print a group of lines
printGroup :: [BL.ByteString] -> IO ()
printGroup [] = return ()
printGroup group = do
    let parsedData = foldl updateEntry emptyEntry (map parseLine group)
    putStrLn $ formatEntry parsedData
