{-# LANGUAGE OverloadedStrings #-}

module DiaguardParser
    ( EntryData(..)
    , emptyEntry
    , startsWithEntry
    , groupByEntry
    , parseLine
    , updateEntry
    , formatEntry
    , splitOnSemicolon
    , removeQuotes
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isPrefixOf)

data EntryData = EntryData
    { entryDate :: String
    , bloodSugar :: Maybe Int
    , fastInsulin :: Maybe Int
    , basalInsulin :: Maybe Int
    , carbs :: Maybe Int
    } deriving (Show, Eq)

emptyEntry :: EntryData
emptyEntry = EntryData "" Nothing Nothing Nothing Nothing

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

-- Format entry data as CSV
formatEntry :: EntryData -> String
formatEntry entry = 
    let dateStr = entryDate entry
        bloodSugarStr = maybe "" show (bloodSugar entry)
        fastInsulinStr = maybe "" show (fastInsulin entry)
        basalInsulinStr = maybe "" show (basalInsulin entry)
        carbsStr = maybe "" show (carbs entry)
    in dateStr ++ "," ++ bloodSugarStr ++ "," ++ fastInsulinStr ++ "," ++ basalInsulinStr ++ "," ++ carbsStr

-- Update entry data with parsed line
updateEntry :: EntryData -> Maybe (EntryData -> EntryData) -> EntryData
updateEntry entry Nothing = entry
updateEntry entry (Just f) = f entry

-- Parse a single line and return an update function
parseLine :: BL.ByteString -> Maybe (EntryData -> EntryData)
parseLine line = 
    let lineStr = BLC.unpack line
        fields = splitOnSemicolon lineStr
    in case fields of
        -- "entry";"2023-12-13 17:27:12";"" -> extract date
        ("\"entry\"":date:_) -> 
            Just (\e -> e { entryDate = removeQuotes date })
        
        -- "measurement";"bloodsugar";"224.0" -> extract and convert to int
        ("\"measurement\"":"\"bloodsugar\"":value:_) -> 
            let bs = floor (read (removeQuotes value) :: Double) :: Int
            in Just (\e -> e { bloodSugar = Just bs })
        
        -- "measurement";"insulin";"5.0";"4.0";"3.0" -> sum first two, get third
        ("\"measurement\"":"\"insulin\"":val1:val2:val3:_) -> 
            let v1 = read (removeQuotes val1) :: Double
                v2 = read (removeQuotes val2) :: Double
                v3 = read (removeQuotes val3) :: Double
                fast = floor (v1 + v2) :: Int
                basal = floor v3 :: Int
            in Just (\e -> e { fastInsulin = Just fast, basalInsulin = Just basal })
        
        -- "measurement";"meal";"60.0" -> extract and convert to int
        ("\"measurement\"":"\"meal\"":value:_) -> 
            let m = floor (read (removeQuotes value) :: Double) :: Int
            in Just (\e -> e { carbs = Just m })
        
        -- Unknown format, ignore
        _ -> Nothing

-- Helper function to split on semicolon
splitOnSemicolon :: String -> [String]
splitOnSemicolon s = case break (== ';') s of
    (field, "") -> [field]
    (field, _:rest) -> field : splitOnSemicolon rest

-- Helper function to remove surrounding quotes
removeQuotes :: String -> String
removeQuotes ('"':rest) = case reverse rest of
    ('"':revRest) -> reverse revRest
    _ -> '"' : rest
removeQuotes s = s
