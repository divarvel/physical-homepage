{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model (Talk(..))
import Talks (talks)
import Views.MainPage
import Views.TalksPage

main = scotty 8080 $ do
  middleware $ staticPolicy (hasPrefix  "assets/")
  get "/" $
    html $ renderHtml $ mainPage talks
  get "/talks" $
      html $ renderHtml $ talksPage talks
