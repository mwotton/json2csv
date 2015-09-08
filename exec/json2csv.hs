{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad        (filterM)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Either          (partitionEithers)
import           Data.Monoid          ((<>))
import           Data.String          (IsString)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Directory
import           System.Environment   (getArgs)
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



main :: IO ()
main = do
  files <- concat <$> (mapM immediateDescendents =<< getArgs)
  (_bad,(good::[Value])) <-
    partitionEithers <$> mapM (\x -> eitherDecode <$> BL.readFile x) files

  let allRows = map (json2CSV defaults) good
      headers = getHeaders allRows
  TIO.putStrLn $ T.intercalate "," headers
  (`mapM_` allRows) (TIO.putStrLn . formatLine headers)
