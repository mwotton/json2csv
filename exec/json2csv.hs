{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative  ((<$>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Either          (partitionEithers)
import           Data.List            (nub)
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Environment   (getArgs)
import           Text.Json2CSV

main = do
  (bad,(good::[Value])) <- partitionEithers <$>
                           (mapM (\x -> eitherDecode <$> BL.readFile x) =<< getArgs)
  let all = map json2CSV good
      headers = nub $ concat $ map (map fst) all
  showLine headers
  (`mapM_` all) $ \l ->
    showLine $  (`map` headers) $ \h -> maybe "" tshow $ lookup h l

showLine = TIO.putStrLn . T.intercalate ","
