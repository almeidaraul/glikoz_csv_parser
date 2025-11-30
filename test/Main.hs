module Main (main) where

import Test.Hspec
import qualified DiaguardParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DiaguardParser" DiaguardParserSpec.spec
