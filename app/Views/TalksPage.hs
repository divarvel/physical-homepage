module Views.TalksPage where

import qualified Text.Blaze.Html5 as H

import           Model            (Talk (..))
import           Views.Layout
import           Views.MainPage   (allTalks)

talksPage :: [Talk] -> H.Html
talksPage talks = mainLayout $
  allTalks False talks
