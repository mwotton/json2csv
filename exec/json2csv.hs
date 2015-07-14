{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative  ((<$>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Either          (partitionEithers)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Environment   (getArgs)
import           Text.Json2CSV

main :: IO ()
main = do
  (_bad,(good::[Value])) <- partitionEithers <$>
                           (mapM (\x -> eitherDecode <$> BL.readFile x) =<< getArgs)
  let allRows = map (json2CSV defaults) good
      headers = getHeaders allRows
  TIO.putStrLn $ T.intercalate "," headers
  (`mapM_` allRows) (TIO.putStrLn . formatLine headers)
