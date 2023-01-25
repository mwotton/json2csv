{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes       #-}
module Text.Json2CSVSpec where
import           Data.Aeson.QQ
import           Test.Hspec
import           Text.Json2CSV
import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Vector(Vector)
-- import Control.DeepSeq(force)
import System.Directory(listDirectory)
import Data.Maybe(isJust)
import System.Timeout(timeout)

main :: IO ()
main = hspec spec


formatLine :: [T.Text] -> [(T.Text,CVal)] -> BL.ByteString
formatLine headers t = CSV.encode [flattenValues headers t]

spec :: Spec
spec = describe "json2csv" $ do
  it "handles embedded quotes sanely" $ do
    let input = [aesonQQ|
                 {
                   "meta": {
                      "keywords": "\u003cscript type=\"text/javascript\"\u003e\r\n\r\n  var _gaq = _gaq || [];\r\n  _gaq.push(['_setAccount', 'UA-37952915-1']);\r\n  _gaq.push(['_trackPageview']);\r\n\r\n  (function() {\r\n    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.as"
                      }
                 }
                        |]
        res = json2CSV defaults input
        final = formatLine (getHeaders [res]) res
    final `shouldBe` "\"\\u003cscript type=\"\"text/javascript\"\"\\u003e\r\n\r\n  var _gaq = _gaq || [];\r\n  _gaq.push(['_setAccount', 'UA-37952915-1']);\r\n  _gaq.push(['_trackPageview']);\r\n\r\n  (function() {\r\n    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.as\"\r\n"
  it "converts jsonlines properly" $ do
    let config = Config
          { justLines = True
          , printHeaders = True
          , shouldExpand = False
          }
    csv <- BL.unlines <$> runConversion config (Right ["./fixtures/sample.jl"])
    BL.putStr csv
    let decoded :: Vector [String] = either error id $ CSV.decode CSV.HasHeader csv
    mapM_ print decoded
    length decoded `shouldBe` 3
    (`mapM_` decoded) $ \x ->
      length x `shouldBe` 14

  -- | The intent of this is that we should be able to process large files efficiently
  --   Not intended for normal use.
  xit "completes with specified json files in 10 seconds" $ do

    r <- timeout 100000000 $ do
      let base = "./fixtures/performance/"
      files <- listDirectory base
      let config = Config
              { justLines = True
              , printHeaders = True
              , shouldExpand = False
              }
      print files
      csv <- BL.unlines <$> runConversion config (Right $ map (base <>) files)
      print csv
      BL.writeFile "/tmp/output" csv
      pure files

    r `shouldSatisfy` isJust
    print r
