{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Json2CSV.Reference

where

import Data.Text.Encoding(encodeUtf8)
import           System.Directory
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Scientific
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import Data.Maybe(fromMaybe)
import           Data.String                (IsString)
import System.IO(hClose,utf8,hSetEncoding)
import qualified Data.Csv as CSV
import System.IO.Temp(openTempFile)
import           Data.Either                (partitionEithers)
import           Control.Monad              (filterM,forM_)
import qualified Streaming.Cassava as SCSV
import Control.Monad.Trans.Resource(runResourceT,MonadResource)
import Control.Monad.IO.Class(liftIO,MonadIO)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as SP
import qualified Data.HashSet as HS

data Config = Config
  { justLines :: Bool
  , printHeaders :: Bool
  , shouldExpand :: Bool
  }

data CVal = CStr Text
          | CNumber Scientific
  deriving Eq
instance CSV.ToField CVal where
  toField = \case
    CStr t -> CSV.toField t
    CNumber s -> CSV.toField s

newtype Row = Row [CVal]

instance CSV.ToRecord Row where
  toRecord (Row cvals) = CSV.record $ map CSV.toField cvals

mAXFIELDLENGTH :: Int
mAXFIELDLENGTH=1000

flattenValues :: [T.Text] -> V.Vector (T.Text,CVal) -> Row
flattenValues headers t =  Row (map  (\h -> fromMaybe (CStr "")  $ fmap snd $ V.find ((==h) . fst) t) headers)

getHeaders :: [V.Vector (T.Text,CVal)] -> [T.Text]
getHeaders allRows = HS.toList $ HS.unions $ fmap (HS.fromList . V.toList . fmap fst) allRows


data Defaults = Defaults {
  nullValue       :: Text,
  yesValue        :: Text,
  noValue         :: Text,
  concatCharacter :: Text,
  collapsible     :: Bool
}

defaults :: Defaults
defaults = Defaults "" "yes" "no" "|" True

json2CSV :: Defaults -> Value -> V.Vector (Text, CVal)
json2CSV config z = V.fromList $ go z []
  where
    go (Object o) p   = concatMap (\(k, v) -> go v (K.toText k:p)) $ KM.toList o
    go (Array a) p    = case mapM (leaf config) (V.toList a) of
      -- if we're at the final level, we can collapse with
      Just x ->         [(mkPath p, CStr (T.intercalate (concatCharacter config) x))]
      Nothing -> concat $ zipWith (\i v -> go v ((tshow i):p))  [(0::Int)..] $ V.toList a
--     go (Array a) p    = concatMap (concat $ zipWith (\i v -> go v ((tshow i):p))  [(0::Int)..] $ V.toList a
    go (String s) p   = [(mkPath p, CStr s)]
    go (Number n) p   = [(mkPath p, CNumber n)]
    go (Bool True) p  = [(mkPath p, CStr (yesValue config))] -- the stupid, it burns.
    go (Bool False) p = [(mkPath p, CStr (noValue config))] -- the stupid, it burns.
    go Null p         = [(mkPath p, CStr (nullValue config))] -- the stupid, it burns.


    mkPath = T.intercalate "."  . reverse

tshow :: Show a => a -> Text
tshow = T.pack . show

leaf :: Defaults -> Value -> Maybe Text
leaf config s
  | collapsible config = case s of
    String str -> Just str
    Number n   -> Just $ tshow n
    Bool True   -> Just $ yesValue config
    Bool False  -> Just $ noValue config
    Null -> Just $ nullValue config
    _ -> Nothing
  | otherwise = Nothing



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

-- Collect the streaming interface into a big lazy list of bytestrings, throwing away
-- any space safety.
runConversion :: Config -> (Either BL.ByteString [FilePath]) -> IO BL.ByteString
runConversion c args = runResourceT $ Q.toLazy_ $ runConversionStreaming c args

runConversionStreaming :: MonadResource m => Config -> (Either BL.ByteString [FilePath]) -> Q.ByteStream m ()
runConversionStreaming Config{..} args = do

  -- consistentJSON <- isJust . lookup "CONSISTENT_JSON" <$> getEnvironment
  -- if we are reading stdin, just copy it to a temporary file
  --
  --   the continuation structure is so that we only open a temporary file
  --   when we need it, but also to guarantee the file is around the whole time
  --   it is needed.

  case args of
    Left input -> do
      (_,fp,handle) <- openTempFile Nothing "json2csv.json"
      -- hSetEncoding stdin utf8
      liftIO $ do
        hSetEncoding handle utf8
        BL.hPutStr handle input
        hClose handle
      continue [fp]
    Right xs -> do
      -- not worth trying to stream this, it's metadata
      continue . concat =<< liftIO (mapM immediateDescendents xs)

  where
    continue :: MonadIO m => [FilePath] -> Q.ByteStream m () -- IO [BL.ByteString]
    continue filepaths = do
      -- this looks weird, but it lets us read the same file in lazily twice.
      -- we'll still have to read the whole thing, because headers could be added
      -- on the last line, but it won't all be in memory.
      (headers,allRows) <- liftIO $ do
        headers <- getHeaders <$> readJSONObjects justLines shouldExpand filepaths
        allRows <- readJSONObjects justLines shouldExpand filepaths
        -- this should probably be a stream
        pure (headers,allRows)
      -- here we get a bit clever and write using the cassava-streaming package instead
      -- of relying on the user not doing something silly like asking for the length then
      -- mapping over a huge file.
      --
      -- technically we're still being a bit silly using a big lazy list internally,
      -- but at least we've policed other people's behaviour.
      SCSV.encode (Just $ SCSV.header $ fmap encodeUtf8 headers)
        (forM_ allRows $ \r -> (SP.yield $ flattenValues headers r))


expand :: Value -> [Value]
expand (Array o) = V.toList o
expand x = error (show x)

readJSONObjects :: Traversable t =>
                  Bool -> Bool -> t FilePath -> IO [V.Vector (T.Text, CVal)]
readJSONObjects justlines shouldExpand filepaths = do

  contents <- concatMap
             (if justlines
              then BL.lines
              else pure)
    <$> mapM BL.readFile filepaths


  let (_bad,(good'::[Value])) = partitionEithers $ map eitherDecode contents

  -- if we define EXPAND, we assume the top level is an array and
  -- expand it out.
  let good =
        if shouldExpand
        then concatMap expand good'
        else good'
  pure $ map  (json2CSV defaults) good
