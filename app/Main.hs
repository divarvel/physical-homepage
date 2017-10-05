{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                   (when)
import           Network.Wai.Metrics             (WaiMetrics, metrics,
                                                  registerWaiMetrics)
import           Network.Wai.Middleware.Static   (hasPrefix, staticPolicy)
import           System.Environment              (lookupEnv)
import           System.Metrics                  (newStore, registerGcMetrics)
import           System.Remote.Monitoring.Statsd (defaultStatsdOptions,
                                                  forkStatsd)
import           Text.Blaze.Html.Renderer.Text   (renderHtml)
import qualified Text.Blaze.Html5                as H
import           Web.Scotty

import           Evman                           (loadTalks)
import           Model                           (Talk (..))
import           Talks                           (talks)
import           Views.MainPage
import           Views.TalksPage


handleMetrics :: IO WaiMetrics
handleMetrics = do
  store <- newStore
  registerGcMetrics store
  waiMetrics <- registerWaiMetrics store
  sendMetrics <- maybe False (== "true") <$> lookupEnv "ENABLE_METRICS"
  when sendMetrics $ do
    putStrLn "statsd reporting enabled"
    forkStatsd defaultStatsdOptions store
    return ()
  return waiMetrics

main = do
  waiMetrics <- handleMetrics
  port <- lookupEnv "PORT"
  talks <- loadTalks
  scotty (maybe 3001 read port) $ do
    middleware $ metrics waiMetrics
    middleware $ staticPolicy (hasPrefix  "assets/")
    middleware $ staticPolicy (hasPrefix  "me/assets/")
    get "/" $
      html $ renderHtml $ mainPage talks
    get "/talks" $
      html $ renderHtml $ talksPage talks
    get "/me" $
      html $ renderHtml $ mainPage talks
    get "/me/talks" $
      html $ renderHtml $ talksPage talks
