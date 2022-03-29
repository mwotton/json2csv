{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module MainEntry where

import           Control.Monad              (filterM)
import           Control.Monad              (when)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                (partitionEithers)
import           Data.Maybe                 (isJust, isNothing)
import           Data.String                (IsString)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Vector                as V
import           System.Directory
import           System.Environment         (getArgs, getEnvironment)
import           Text.Json2CSV
import qualified Data.Csv as CSV
import System.IO(hClose,hPutStr)
import System.IO.Temp(withSystemTempFile)

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


-- | Two separate modes
--   If there are arguments passed, we process all of them.
--   If there are no arguments, we assume we are just reading from stdin.
main :: IO ()
main = do
  args <- getArgs
  justlines <- isJust . lookup "JUST_LINES" <$> getEnvironment
  printHeaders <- isNothing . lookup "NO_HEADERS" <$> getEnvironment
  shouldExpand <- isJust . lookup "EXPAND" <$> getEnvironment
  -- consistentJSON <- isJust . lookup "CONSISTENT_JSON" <$> getEnvironment
  -- if we are reading stdin, just copy it to a temporary file
  withSystemTempFile "json2csv.json" $ \fp handle -> do
    filepaths <- case args of
      [] -> do
        hPutStr handle =<< getContents
        hClose handle
        pure [fp]
      xs -> do
        hClose handle
        concat <$> mapM immediateDescendents xs

    -- this looks weird, but it lets us read the same file in lazily twice.
    headers <- getHeaders <$> readJSONObjects justlines shouldExpand filepaths

    allRows <- readJSONObjects justlines shouldExpand filepaths
    when printHeaders $ TIO.putStrLn $ T.intercalate "," headers
    BL.putStr $ CSV.encode (map (flattenValues headers) allRows :: [Row])

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
