{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
module Text.Json2CSVFast where

import Data.Foldable(toList)
import Data.Text.Encoding(encodeUtf8)
import           System.Directory
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import Data.Maybe(fromMaybe)
import           Data.String                (IsString)
import System.IO(openFile,IOMode(ReadMode))
import qualified Data.Csv as CSV
import           Control.Monad              (filterM,forM_)
import qualified Streaming.Cassava as SCSV
import Control.Monad.IO.Class(liftIO,MonadIO)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as SP
import qualified Data.HashSet as HS
import Json(Value)
import qualified Json as JsonSyntax
import qualified Data.Bytes as Bytes
import qualified Data.Text.Short as Short
import qualified Data.Number.Scientific as Scientific
import Data.Word
import Data.Char
import qualified Data.HashMap.Strict as HM

data Config = Config
  { justLines :: Bool
  , printHeaders :: Bool
  , shouldExpand :: Bool
  }

data CVal = CStr Short.ShortText
          | CNumber Scientific.Scientific
  deriving (Eq,Show)
instance CSV.ToField CVal where
  toField = \case
    CStr t -> CSV.toField t
    -- this will eventually start using scientific notation, which
    -- is arguably problematic. TODO https://github.com/andrewthad/scientific-notation/issues/5
    CNumber s -> CSV.toField $ Scientific.encode s

newtype Row = Row (V.Vector CVal)

instance CSV.ToRecord Row where
  toRecord (Row cvals) = CSV.record $ map CSV.toField $ V.toList cvals

mAXFIELDLENGTH :: Int
mAXFIELDLENGTH=1000

flattenValues :: (HM.HashMap T.Text Int, V.Vector CVal) -> V.Vector (T.Text,CVal) -> Row
flattenValues (headers, baseRow)
  = Row . V.update baseRow . fmap (\(k,c) -> (fromMaybe (error "bad index") $ HM.lookup k headers, c))

data Defaults = Defaults {
  nullValue       :: Short.ShortText,
  yesValue        :: Short.ShortText,
  noValue         :: Short.ShortText,
  concatCharacter :: Short.ShortText,
  collapsible     :: Bool
}

defaults :: Defaults
defaults = Defaults "" "yes" "no" "|" True

tshow :: Show a => a -> Text
tshow = T.pack . show

makeRelative :: (Monoid m, IsString m) => m -> m -> m
makeRelative d f = d <> "/" <> f

immediateDescendents :: FilePath -> IO [FilePath]
immediateDescendents fp = do
  isFile <- doesFileExist fp
  if isFile
    then return [fp]
    else do
      isDir <- doesDirectoryExist fp
      if isDir
        then filterM doesFileExist . map (makeRelative fp) =<< getDirectoryContents fp
        else return []

-- Collect the streaming interface into a big lazy bytestring, throwing away
-- any space safety.
runConversion :: Config -> [FilePath] -> IO BL.ByteString
runConversion c args = -- fmap BL.fromChunks $ runResourceT $ SP.toList_
  Q.toLazy_
  $ runConversionStreaming c args

runConversionStreaming :: MonadIO m
  => Config
  -> [FilePath]
  -> Q.ByteStream m (SCSV.Header, Int)
runConversionStreaming Config{..} paths  = continue paths
  where
    -- I thought really hard about how to do this with streams but just failed.
    -- The difficulty is that you need the computed headers in order to start writing a CSV:
    -- fundamentally, this means you need to be able to consume the whole stream twice.
    --
    -- if it were possible to rewind the stream and prefix the headers, we could do it, but I don't
    -- think that's even really available at the POSIX layer.
    continue :: MonadIO m => [FilePath] -> Q.ByteStream m (SCSV.Header,Int)
    continue filepaths = do
      let  generator :: MonadIO m => SP.Stream (SP.Of (V.Vector (T.Text,CVal))) m () -- (SCSV.Header,Integer)
           generator = readJSONObjects justLines shouldExpand filepaths
      headers :: [Text] <- HS.toList <$> SP.fold_ (\hs v -> foldr (\(k,_) h' -> HS.insert k h') hs v) mempty id generator
      let headerTable = (HM.fromList $ (`zip` [0..]) headers, V.replicate (length headers) (CStr ""))

      -- reuse generator
      let go :: MonadIO m => SP.Stream (SP.Of Row) m (SCSV.Header,Int)
          go = (V.fromList (map encodeUtf8 headers),) . SP.fst' <$>  SP.store SP.length (SP.map (flattenValues headerTable) generator)


      SCSV.encode (Just $ SCSV.header $ fmap encodeUtf8 headers) $ go

-- there might be a little more to squeeze out here by adopting a builder interface rather
-- than naively appending Bytes values, but if the buffer is appreciably bigger than most
-- individual lines, concatenations are going to be rare anyway. Most of the time is still
-- spent in JSON decoding, so not worth it yet.
fetchStringsFromFile :: MonadIO m => Bool -> Int -> FilePath -> SP.Stream (SP.Of Bytes.Bytes) m ()
fetchStringsFromFile False _bufsize fp = SP.yield =<< liftIO (Bytes.readFile fp)
fetchStringsFromFile True bufsize fp = do
  handle <- liftIO $ openFile  fp ReadMode
  -- from jsonlines.org
  -- 3. Line Separator is '\n'
  -- This means '\r\n' is also supported because surrounding white space is implicitly ignored when parsing JSON values.
  --
  -- so while this does look a little sketchy, the json decoder should be able to handle a trailing '\r' without problems.
  let nl :: Word8 = fromIntegral $ ord '\n'
  let go buf = do
        newBytes <- liftIO $ Bytes.hGet handle bufsize
        case Bytes.splitEnd1 nl newBytes of
          Just (front, back) -> do
            -- split, but with remnants added at the front
            forM_ (Bytes.split nl $ buf <> front) SP.yield
            go back
          Nothing -> do
            let newbuf = buf <> newBytes
            if Bytes.length newBytes == bufsize
            then
              -- normal read that happens to not contain a newline, so we add to the buffer.
              go newbuf
            else
              -- we got less than the expected count, so we are at the end of the file.
              -- only yield non-empty lines.
              if Bytes.null newbuf
              then pure ()
              else SP.yield newbuf

  go Bytes.empty

-- |Just an interface. Eventually we'll put a parallel mapper in here.
mapper :: MonadIO m
       => (a -> b)
       -> SP.Stream (SP.Of a) m r
       -> SP.Stream (SP.Of b) m r
mapper = SP.map

readJSONObjects :: (Traversable t,MonadIO m) =>
                  Bool -> Bool -> t FilePath -> SP.Stream (SP.Of (V.Vector (T.Text,CVal))) m ()
readJSONObjects justlines shouldExpand filePaths = do
  forM_ filePaths $ \fp -> do
    SP.map (json2CSV defaults)
      $ expander
--      $ SP.map (\x -> either (\y -> error (show (y,Bytes.toByteString x))) id . JsonSyntax.decode $ x)
      $ mapper (\x -> either (\y -> error (show (y,Bytes.toByteString x))) id . JsonSyntax.decode $ x)

      $ fetchStringsFromFile justlines 16777216 fp

  where
      expander :: Monad m
               => SP.Stream (SP.Of JsonSyntax.Value) m ()
               -> SP.Stream (SP.Of JsonSyntax.Value) m ()
      expander x =
          if shouldExpand
          then SP.concat $ SP.map expand x
          else x

json2CSV :: Defaults -> JsonSyntax.Value -> V.Vector (Text, CVal)
json2CSV config z = V.fromList $ map (\(a,b) -> (Short.toText a, b)) $ go z []
  where
    go :: JsonSyntax.Value -> [Short.ShortText] -> [(Short.ShortText,CVal)]
    go val p = case val of
      JsonSyntax.Object o -> concatMap (\(JsonSyntax.Member k v) -> go v (k:p)) $ toList o
      JsonSyntax.Array a -> case mapM leaf (toList a) of
                             -- if we're at the final level, we can collapse with
                             Just x ->         [(mkPath p, CStr (Short.intercalate (concatCharacter config) x))]
                             Nothing -> concat $ zipWith (\i v -> go v ((shortshow i):p))  [(0::Int)..] $ toList a
      JsonSyntax.String s -> [(mkPath p, CStr s)]
      JsonSyntax.Number n -> [(mkPath p, CNumber n)]
      JsonSyntax.Null ->  [(mkPath p, CStr (nullValue config))] -- the stupid, it burns.
      JsonSyntax.True -> [(mkPath p, CStr (yesValue config))] -- the stupid, it burns.
      JsonSyntax.False -> [(mkPath p, CStr (yesValue config))] -- the stupid, it burns.


    mkPath = Short.intercalate "."  . reverse


    leaf :: JsonSyntax.Value -> Maybe Short.ShortText
    leaf s
      | collapsible config = case s of
        JsonSyntax.String str -> Just str
        JsonSyntax.Number n   -> Just $ shortshow n
        JsonSyntax.True ->  Just $ yesValue config
        JsonSyntax.False -> Just $ noValue config
        JsonSyntax.Null -> Just $ nullValue config
        _ -> Nothing
      | otherwise = Nothing

shortshow :: Show a => a -> Short.ShortText
shortshow = Short.pack . show

expand :: Value -> [Value]
expand (JsonSyntax.Array o) = toList o
expand x = error (show x)
