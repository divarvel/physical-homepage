module Model where

import Data.Text (Text(..))

data Talk = Talk
  { title       :: !Text
  , description :: !Text
  , slides      :: !(Maybe Text)
  , featured    :: !Bool
  }
