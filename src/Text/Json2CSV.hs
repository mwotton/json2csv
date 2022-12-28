{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Json2CSV where


import           System.Directory
import Data.Text.Encoding(encodeUtf8)
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

--import qualified Data.HashMap.Strict as HM
import           Data.List           (nub)
import qualified Data.ByteString.Lazy.Char8 as BL
-- import           Data.Monoid         ((<>))
import           Data.Scientific
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
-- import qualified Data.Csv as CSV
import Data.Maybe(fromMaybe)
import           Data.String                (IsString)
import System.IO(hClose,utf8,hSetEncoding,stdin)
import qualified Data.Csv as CSV
import System.IO.Temp(withSystemTempFile)
import           Data.Either                (partitionEithers)

import           Control.Monad              (filterM)

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

flattenValues :: [T.Text] -> [(T.Text,CVal)] -> Row
flattenValues headers t =  Row (map  (\h -> fromMaybe (CStr "")  $ lookup h t) headers)

getHeaders :: [[(T.Text,CVal)]] -> [T.Text]
getHeaders allRows = nub $ concat $ map (map fst) allRows



data Defaults = Defaults {
  nullValue       :: Text,
  yesValue        :: Text,
  noValue         :: Text,
  concatCharacter :: Text,
  collapsible     :: Bool
}

defaults :: Defaults
defaults = Defaults "" "yes" "no" "|" True

json2CSV :: Defaults -> Value -> [(Text, CVal)]
json2CSV config z = go z []
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

runConversion :: Config -> [FilePath] -> IO [BL.ByteString]
runConversion Config{..} args = do

  -- consistentJSON <- isJust . lookup "CONSISTENT_JSON" <$> getEnvironment
  -- if we are reading stdin, just copy it to a temporary file
  -- | Two separate modes
  --   If there are arguments passed, we process all of them.
  --   If there are no arguments, we assume we are just reading from stdin.
  withSystemTempFile "json2csv.json" $ \fp handle -> do
    filepaths <- case args of
      [] -> do
        hSetEncoding stdin utf8
        hSetEncoding handle utf8
        BL.hPutStr handle =<< BL.getContents
        hClose handle
        pure [fp]
      xs -> do
        hClose handle
        concat <$> mapM immediateDescendents xs

    -- this looks weird, but it lets us read the same file in lazily twice.
    headers <- getHeaders <$> readJSONObjects justLines shouldExpand filepaths

    allRows <- readJSONObjects justLines shouldExpand filepaths
    let headerRow = if printHeaders
          then [BL.fromStrict $ encodeUtf8 $ T.intercalate "," headers] :: [BL.ByteString]
          else []

    pure $ headerRow <> [CSV.encode (map (flattenValues headers) allRows :: [Row])]



readJSONObjects :: Traversable t =>
                  Bool -> Bool -> t FilePath -> IO [[(T.Text, CVal)]]
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
  pure $ map (json2CSV defaults) good


expand :: Value -> [Value]
expand (Array o) = V.toList o
expand x = error (show x)
