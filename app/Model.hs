{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Data.Char           (isAlpha)
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Normalize


data Lang = Fr | En deriving (Eq, Ord)

type Item = (Maybe Text, Text)

data Talk = Talk
  { title       :: !Text
  , description :: !Text
  , slides      :: !(M.Map Lang (NonEmpty Item))
  , video       :: !(M.Map Lang (NonEmpty Item))
  , featured    :: !Bool
  }

makeSlug :: Text -> Text
makeSlug title' = withDashes
  where
    lowercase = T.toLower title'
    normalized = normalize NFD lowercase
    usable c = (c == ' ') || isAlpha c
    filtered = T.filter usable normalized
    withDashes = T.replace " " "-" filtered
