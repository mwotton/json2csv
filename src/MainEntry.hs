{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module MainEntry where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (isJust, isNothing)
import           System.Environment         (getArgs, getEnvironment)
import           Text.Json2CSV


main :: IO ()
main = do
  args <- getArgs
  justLines <- isJust . lookup "JUST_LINES" <$> getEnvironment
  printHeaders <- isNothing . lookup "NO_HEADERS" <$> getEnvironment
  shouldExpand <- isJust . lookup "EXPAND" <$> getEnvironment

  output <- runConversion Config{..} args
  BL.putStr output
