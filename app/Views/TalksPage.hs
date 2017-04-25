{-# LANGUAGE OverloadedStrings #-}

module Views.TalksPage where

import           Control.Monad                 (forM_)
import           Data.Monoid                   (mconcat, (<>))
import           Data.Text                     (Text (..))
import           Network.Wai.Middleware.Static (static)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (toHtml, toValue, (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

import           Model                         (Talk (..))
import           Views.Layout
import           Views.MainPage                (allTalks)

talksPage :: [Talk] -> H.Html
talksPage talks = mainLayout $
  allTalks False talks
