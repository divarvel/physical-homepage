{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Data.Char               (isAlpha)
import qualified Data.Map                as M
import           Data.Text               (Text (..))
import qualified Data.Text               as T
import           Data.Text.ICU.Char
import           Data.Text.ICU.Normalize


data Lang = Fr | En deriving (Eq, Ord)

data Talk = Talk
  { title       :: !Text
  , description :: !Text
  , slides      :: !(M.Map Lang Text)
  , video       :: !(M.Map Lang Text)
  , featured    :: !Bool
  }

makeSlug :: Text -> Text
makeSlug title = withDashes
  where
    lowercase = T.toLower title
    normalized = normalize NFD lowercase
    usable c = (c == ' ') || isAlpha c
    filtered = T.filter usable normalized
    withDashes = T.replace " " "-" filtered
