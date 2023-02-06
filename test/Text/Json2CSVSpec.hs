{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes       #-}
module Text.Json2CSVSpec where
import           Test.Hspec
import           Text.Json2CSVFast
import qualified Text.Json2CSV.Reference as Reference
import qualified Data.Csv as CSV
import Data.Vector(Vector)
import qualified Data.Vector as V
import System.Directory(listDirectory)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as SP
import Control.Monad.Trans.Resource(runResourceT)
import System.Environment(lookupEnv)
import Control.Monad.IO.Class(liftIO)
import Control.Monad(forM_)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

main :: IO ()
main = hspec spec

tokenise :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
    where (h,t) = BS.breakSubstring x y

spec :: Spec
spec = describe "json2csv" $ do
  it "does the same thing for old and new implementations" $ do
    let config = Config
          { justLines = True
          , printHeaders = True
          , shouldExpand = False
          }
    let c2 = Reference.Config
          { Reference.justLines = True
          , Reference.printHeaders = True
          , Reference.shouldExpand = False
          }

    forM_ ["./fixtures/sample.jl"] $ \(fname ::String) -> do
      csv <- liftIO $ runConversion config [fname]
      oldCsv <- liftIO $ Reference.runConversion c2 (Right [fname])
      -- this is a bit of a hack, but the new CSV library doesn't
      -- append a decimal point on integer values.
      csv `shouldBe` (BL.fromStrict $ BS.intercalate "," $ tokenise ".0," $ BL.toStrict $ oldCsv)




  it "converts jsonlines properly" $ do
    let config = Config
          { justLines = True
          , printHeaders = True
          , shouldExpand = False
          }
    csv <- runConversion config ["./fixtures/sample.jl"]
    let decoded :: Vector [String] = either error id $ CSV.decode CSV.NoHeader csv

    length decoded `shouldBe` 4 -- including header
    decoded V.! 0 `shouldBe` ["Schedule","Job ID","Sub Category","Budget","State","Status","Description","PostedOriginal","Title","Parent Category","_type","Posted","Suburb"]
    (`mapM_` decoded) $ \x ->
      length x `shouldBe` 13

  it "fetch strings works" $ do
    r <- SP.sum_ $ SP.map (const (1::Int)) $ fetchStringsFromFile  True 1024 "fixtures/performance/jldata.jl"
    r `shouldBe` 353796
  -- | The intent of this is that we should be able to process large files efficiently
  --   Not intended for normal testing.
  it "completes with specified jsonlines files" $ do
    liftIO (lookupEnv "RUN_PERFTEST") >>= \case
      Nothing -> pendingWith "set RUN_PERFTEST environment variable if you want to check the slow tests."
      Just _ -> do

        let base = "./fixtures/performance/"
        files <- listDirectory base
        let config = Config
                { justLines = True
                , printHeaders = True
                , shouldExpand = False
                }
        runResourceT $ Q.writeFile "/tmp/output"  $ Q.fromChunks $ runConversionStreaming config ( map (base <>) files)
