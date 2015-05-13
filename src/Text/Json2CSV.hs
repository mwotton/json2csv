{-# LANGUAGE OverloadedStrings #-}
module Text.Json2CSV where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Monoid         ((<>))
import           Data.Scientific
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V

data CVal = CStr Text
          | CNumber Scientific

instance Show CVal where
  show (CStr s) = T.unpack (("\""::Text) <>
                            T.replace "\"" "\"\"" s
                            <> "\"")
  show (CNumber s) = show s

json2CSV :: Value -> [(Text, CVal)]
json2CSV z = go z []
  where
    go (Object o) p   = concatMap (\(k, v) -> go v (k:p)) $ HM.toList o
    go (Array a) p    = concat $ zipWith (\i v -> go v ((tshow i):p))  [(0::Int)..] $ V.toList a
    go (String s) p   = [(mkPath p, CStr s)]
    go (Number n) p   = [(mkPath p, CNumber n)]
    go (Bool True) p  = [(mkPath p, CStr "yes")] -- the stupid, it burns.
    go (Bool False) p = [(mkPath p, CStr "no")] -- the stupid, it burns.
    go Null p         = [(mkPath p, CStr "null")] -- the stupid, it burns.



    mkPath = T.intercalate "." . reverse

tshow :: Show a => a -> Text
tshow = T.pack . show
