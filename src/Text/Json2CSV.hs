{-# LANGUAGE OverloadedStrings #-}
module Text.Json2CSV where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.List           (nub)
-- import           Data.Monoid         ((<>))
import           Data.Scientific
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V

data CVal = CStr Text
          | CNumber Scientific
  deriving Eq

mAXFIELDLENGTH :: Int
mAXFIELDLENGTH=1000

instance Show CVal where
  show (CStr s) = T.unpack ( -- ("\""::Text) <>
                            T.replace "\\\"" "\"\""
                            (T.pack $ show $ T.take mAXFIELDLENGTH s)
--                             <> "\""
                            )
  show (CNumber s) = show s

formatLine :: [T.Text] -> [(T.Text,CVal)] -> T.Text
formatLine headers t = T.intercalate "," $  (map  (\h -> maybe "" tshow $ lookup h t) headers)

getHeaders :: [[(T.Text,CVal)]] -> [T.Text]
getHeaders allRows = nub $ concat $ map (map fst) allRows


data Defaults = Defaults {
  nullValue       :: Text,
  yesValue        :: Text,
  noValue         :: Text,
  concatCharacter :: Text
}

defaults :: Defaults
defaults = Defaults "" "yes" "no" "|"

json2CSV :: Defaults -> Value -> [(Text, CVal)]
json2CSV config z = go z []
  where
    go (Object o) p   = concatMap (\(k, v) -> go v (k:p)) $ HM.toList o
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



    mkPath = T.intercalate "." . reverse

tshow :: Show a => a -> Text
tshow = T.pack . show

leaf :: Defaults -> Value -> Maybe Text
leaf config s = case s of
  String str -> Just str
  Number n   -> Just $ tshow n
  Bool True   -> Just $ yesValue config
  Bool False  -> Just $ noValue config
  Null -> Just $ nullValue config
  _ -> Nothing
