{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Text.Json2CSVSpec where
import           Data.Aeson.QQ
import           Test.Hspec
import           Text.Json2CSV
import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy.Char8 as BL

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

      -- "\"\\\\u003cscript type=\"\"text/javascript\"\"\\\\u003e\\r\\n\\r\\n  var _gaq = _gaq || [];\\r\\n  _gaq.push(['_setAccount', 'UA-37952915-1']);\\r\\n  _gaq.push(['_trackPageview']);\\r\\n\\r\\n  (function() {\\r\\n    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.as\""
