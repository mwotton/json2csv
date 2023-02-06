{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module MainEntry where

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe                 (isJust, isNothing)
import           System.Environment         (getArgs, getEnvironment)
import           Text.Json2CSV
-- import System.IO(utf8,hSetEncoding,stdin)

-- | Two separate modes
--   If there are arguments passed, we process all of them.
--   If there are no arguments, we assume we are just reading from stdin.
mainEntry :: IO ()
mainEntry = do
  cmdlineArgs <- getArgs
  justLines <- isJust . lookup "JUST_LINES" <$> getEnvironment
  printHeaders <- isNothing . lookup "NO_HEADERS" <$> getEnvironment
  shouldExpand <- isJust . lookup "EXPAND" <$> getEnvironment
  args <- case cmdlineArgs of
    [] -> do
      error "todo stdin"
      -- hSetEncoding stdin utf8
      -- Left <$> BL8.getContents
    _ -> pure cmdlineArgs

  output <- runConversion Config{..} args
  BL8.putStr output
