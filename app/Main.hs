{-# LANGUAGE OverloadedStrings #-}


import Network.Wai.Metrics (metrics, registerWaiMetrics)
import Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import System.Metrics (newStore)
import System.Remote.Monitoring.Statsd (defaultStatsdOptions, forkStatsd)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty

import Model (Talk(..))
import Talks (talks)
import Views.MainPage
import Views.TalksPage

main = do
  store <- newStore
  waiMetrics <- registerWaiMetrics store
  forkStatsd defaultStatsdOptions store
  scotty 8080 $ do
    middleware $ metrics waiMetrics
    middleware $ staticPolicy (hasPrefix  "assets/")
    get "/" $
      html $ renderHtml $ mainPage talks
    get "/talks" $
        html $ renderHtml $ talksPage talks
