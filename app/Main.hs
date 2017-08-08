{-# LANGUAGE OverloadedStrings #-}


import Network.Wai.Metrics (metrics, registerWaiMetrics)
import Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import System.Environment (lookupEnv)
import System.Metrics (newStore, registerGcMetrics)
import System.Remote.Monitoring.Statsd (defaultStatsdOptions, forkStatsd)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty

import Evman (loadTalks)
import Model (Talk(..))
import Talks (talks)
import Views.MainPage
import Views.TalksPage

main = do
  store <- newStore
  registerGcMetrics store
  waiMetrics <- registerWaiMetrics store
  forkStatsd defaultStatsdOptions store
  port <- lookupEnv "PORT"
  talks <- loadTalks
  scotty (maybe 3001 read port) $ do
    middleware $ metrics waiMetrics
    middleware $ staticPolicy (hasPrefix  "assets/")
    get "/" $
      html $ renderHtml $ mainPage talks
    get "/talks" $
        html $ renderHtml $ talksPage talks
