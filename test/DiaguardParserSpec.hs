{-# LANGUAGE OverloadedStrings #-}

module DiaguardParserSpec (spec) where

import Test.Hspec
import DiaguardParser
import qualified Data.ByteString.Lazy.Char8 as BLC
import Prelude hiding (lines)

spec :: Spec
spec = do
  describe "removeQuotes" $ do
    it "removes surrounding double quotes" $ do
      removeQuotes "\"hello\"" `shouldBe` "hello"
    
    it "handles strings without quotes" $ do
      removeQuotes "hello" `shouldBe` "hello"
    
    it "handles empty quoted string" $ do
      removeQuotes "\"\"" `shouldBe` ""
    
    it "handles single quote at start only" $ do
      removeQuotes "\"hello" `shouldBe` "\"hello"

  describe "splitOnSemicolon" $ do
    it "splits a simple string" $ do
      splitOnSemicolon "a;b;c" `shouldBe` ["a", "b", "c"]
    
    it "handles empty fields" $ do
      splitOnSemicolon "a;;c" `shouldBe` ["a", "", "c"]
    
    it "handles no semicolons" $ do
      splitOnSemicolon "abc" `shouldBe` ["abc"]
    
    it "handles trailing semicolon" $ do
      splitOnSemicolon "a;b;" `shouldBe` ["a", "b", ""]

  describe "startsWithEntry" $ do
    it "recognizes entry lines" $ do
      startsWithEntry (BLC.pack "\"entry\";\"2023-12-13 17:27:12\";\"\"") `shouldBe` True
    
    it "rejects non-entry lines" $ do
      startsWithEntry (BLC.pack "\"measurement\";\"bloodsugar\";\"224.0\"") `shouldBe` False
    
    it "rejects empty lines" $ do
      startsWithEntry (BLC.pack "") `shouldBe` False

  describe "parseLine" $ do
    it "parses entry line correctly" $ do
      let line = BLC.pack "\"entry\";\"2023-12-13 17:27:12\";\"\""
          result = parseLine line
      case result of
        Just f -> entryDate (f emptyEntry) `shouldBe` "2023-12-13 17:27:12"
        Nothing -> expectationFailure "Expected Just but got Nothing"
    
    it "parses blood sugar correctly" $ do
      let line = BLC.pack "\"measurement\";\"bloodsugar\";\"224.0\""
          result = parseLine line
      case result of
        Just f -> bloodSugar (f emptyEntry) `shouldBe` Just 224
        Nothing -> expectationFailure "Expected Just but got Nothing"
    
    it "parses insulin correctly" $ do
      let line = BLC.pack "\"measurement\";\"insulin\";\"5.0\";\"4.0\";\"3.0\""
          result = parseLine line
      case result of
        Just f -> do
          let entry = f emptyEntry
          fastInsulin entry `shouldBe` Just 9
          basalInsulin entry `shouldBe` Just 3
        Nothing -> expectationFailure "Expected Just but got Nothing"
    
    it "parses meal correctly" $ do
      let line = BLC.pack "\"measurement\";\"meal\";\"60.0\""
          result = parseLine line
      case result of
        Just f -> carbs (f emptyEntry) `shouldBe` Just 60
        Nothing -> expectationFailure "Expected Just but got Nothing"
    
    it "returns Nothing for unknown lines" $ do
      let line = BLC.pack "\"unknown\";\"data\""
          result = parseLine line
      case result of
        Nothing -> return ()
        Just _ -> expectationFailure "Expected Nothing but got Just"
    
    it "handles invalid blood sugar value gracefully" $ do
      let line = BLC.pack "\"measurement\";\"bloodsugar\";\"N/A\""
          result = parseLine line
      case result of
        Nothing -> return ()
        Just _ -> expectationFailure "Expected Nothing for invalid blood sugar"
    
    it "handles invalid insulin values gracefully" $ do
      let line = BLC.pack "\"measurement\";\"insulin\";\"error\";\"4.0\";\"3.0\""
          result = parseLine line
      case result of
        Nothing -> return ()
        Just _ -> expectationFailure "Expected Nothing for invalid insulin"
    
    it "handles invalid meal value gracefully" $ do
      let line = BLC.pack "\"measurement\";\"meal\";\"invalid\""
          result = parseLine line
      case result of
        Nothing -> return ()
        Just _ -> expectationFailure "Expected Nothing for invalid meal"
    
    it "handles empty numeric values gracefully" $ do
      let line = BLC.pack "\"measurement\";\"bloodsugar\";\"\""
          result = parseLine line
      case result of
        Nothing -> return ()
        Just _ -> expectationFailure "Expected Nothing for empty blood sugar"

  describe "formatEntry" $ do
    it "formats complete entry" $ do
      let entry = EntryData "2023-12-13 17:27:12" (Just 224) (Just 9) (Just 3) (Just 60)
      formatEntry entry `shouldBe` "2023-12-13 17:27:12,224,9,3,60"
    
    it "formats entry with missing blood sugar" $ do
      let entry = EntryData "2023-12-13 17:27:12" Nothing (Just 9) (Just 3) (Just 60)
      formatEntry entry `shouldBe` "2023-12-13 17:27:12,,9,3,60"
    
    it "formats entry with missing insulin" $ do
      let entry = EntryData "2023-12-13 17:27:12" (Just 224) Nothing Nothing (Just 60)
      formatEntry entry `shouldBe` "2023-12-13 17:27:12,224,,,60"
    
    it "formats entry with missing meal" $ do
      let entry = EntryData "2023-12-13 17:27:12" (Just 224) (Just 9) (Just 3) Nothing
      formatEntry entry `shouldBe` "2023-12-13 17:27:12,224,9,3,"
    
    it "formats entry with only date" $ do
      let entry = EntryData "2023-12-13 17:27:12" Nothing Nothing Nothing Nothing
      formatEntry entry `shouldBe` "2023-12-13 17:27:12,,,,"

  describe "groupByEntry" $ do
    it "groups lines correctly" $ do
      let lines = map BLC.pack
            [ "\"entry\";\"2023-12-13 17:27:12\";\"\""
            , "\"measurement\";\"bloodsugar\";\"224.0\""
            , "\"measurement\";\"insulin\";\"5.0\";\"4.0\";\"3.0\""
            , "\"entry\";\"2023-12-13 18:00:00\";\"\""
            , "\"measurement\";\"meal\";\"60.0\""
            ]
          groups = groupByEntry lines
      length groups `shouldBe` 2
      length (head groups) `shouldBe` 3
      length (groups !! 1) `shouldBe` 2
    
    it "handles single entry" $ do
      let lines = [BLC.pack "\"entry\";\"2023-12-13 17:27:12\";\"\""]
          groups = groupByEntry lines
      length groups `shouldBe` 1
      length (head groups) `shouldBe` 1
    
    it "handles empty list" $ do
      groupByEntry [] `shouldBe` ([] :: [[BLC.ByteString]])

  describe "updateEntry" $ do
    it "applies update function" $ do
      let entry = emptyEntry
          update = Just (\e -> e { entryDate = "2023-12-13" })
          result = updateEntry entry update
      entryDate result `shouldBe` "2023-12-13"
    
    it "returns entry unchanged when Nothing" $ do
      let entry = emptyEntry { entryDate = "test" }
          result = updateEntry entry Nothing
      result `shouldBe` entry
    
    it "chains multiple updates" $ do
      let entry = emptyEntry
          update1 = Just (\e -> e { entryDate = "2023-12-13" })
          update2 = Just (\e -> e { bloodSugar = Just 224 })
          result = updateEntry (updateEntry entry update1) update2
      entryDate result `shouldBe` "2023-12-13"
      bloodSugar result `shouldBe` Just 224
