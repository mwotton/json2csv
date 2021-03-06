{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad              (filterM)
import           Control.Monad              (when)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                (partitionEithers)
import           Data.Maybe                 (isJust, isNothing)
import           Data.Monoid                ((<>))
import           Data.String                (IsString)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Vector                as V
import           System.Directory
import           System.Environment         (getArgs, getEnvironment)
import           Text.Json2CSV

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
  a <- getArgs
  justlines <- isJust . lookup "JUST_LINES" <$> getEnvironment
  printHeaders <- isNothing . lookup "NO_HEADERS" <$> getEnvironment
  shouldExpand <- isJust . lookup "EXPAND" <$> getEnvironment
  (contents :: [BL.ByteString]) <- case a of
    [] -> (if justlines
          then BL.lines
          else return) <$> BL.getContents
    fs -> mapM BL.readFile . concat =<< mapM immediateDescendents fs


  let (_bad,(good'::[Value])) = partitionEithers $ map eitherDecode contents

  -- if we define EXPAND, we assume the top level is an array and
  -- expand it out.
  let good =
        if shouldExpand
        then concatMap expand good'
        else good'
  let allRows = map (json2CSV defaults) good
      headers = getHeaders allRows
  when printHeaders $ TIO.putStrLn $ T.intercalate "," headers
  (`mapM_` allRows) (TIO.putStrLn . formatLine headers)

expand :: Value -> [Value]
expand (Array o) = V.toList o
expand x = error (show x)
