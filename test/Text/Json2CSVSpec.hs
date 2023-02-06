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
        (headers,runCount) <- runResourceT $ Q.writeFile "/tmp/output" $ runConversionStreaming config ( map (base <>) files)
        headers `shouldBe` (V.fromList ["task.reviews_completed","task.sender_review_present","task.bid_on","task.online_or_phone","task.slug","task.cancellation.accepted_at","task.deadline","task.project","task.runner_id","task.attachment","task.runner_review_present","task.cancellation.rejected_at","task.private_messages_allowed","task.comments_count","task.bids_count","task.next_scheduled_at","task.id","task.payment_status","task.runners_required_count","task.name","task.review_by_sender_id","task.origin_task_slug","task.price","task.prepaid","task.posted_or_edited_at","task.assigned_price","task.cancellation.auto_accept_at","task.allow_calls_runner","task.origin_task_id","task.cancellation.reason","task.state","task.completed_at","task.cancellation","task.was_copied_from_id","task.following","task.created_at","task.estimated_hourly_rate","task.cancellation.rejecter_user_id","task.cancellation.created_at","task.assigned_at","task.estimated_hours","task.first_posted_at","task.cancellation.state","task.first_runners_required_count","task.closed_at","task.description","task.allow_calls_sender","task.amount","task.review_required","task.city","task.cancellation.id","_type","task.sender_id","task.cancellation.requester_user_id","task.payment_type","task.deadline_required","task.active_schedule","task.runners_assigned_count","task.fixed_price","task.out_of_pocket_expenses","task.review_by_runner_id","task.default_location_id"])
        runCount `shouldBe` 353796
