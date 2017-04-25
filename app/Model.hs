module Model where

import qualified Data.Map  as M
import           Data.Text (Text (..))

data Lang = Fr | En deriving (Eq, Ord)

data Talk = Talk
  { title       :: !Text
  , description :: !Text
  , slides      :: !(M.Map Lang Text)
  , video       :: !(M.Map Lang Text)
  , featured    :: !Bool
  }
