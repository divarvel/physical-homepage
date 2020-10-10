{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                   (when)
import           Data.Functor                    (void)
import           Network.Wai.Metrics             (WaiMetrics, metrics,
                                                  registerWaiMetrics)
import           Network.Wai.Middleware.Static   (hasPrefix, staticPolicy)
import           System.Environment              (lookupEnv)
import           System.IO                       (BufferMode (..),
                                                  hSetBuffering, stdin)
import           System.Metrics                  (newStore, registerGcMetrics)
import           System.Remote.Monitoring.Statsd (defaultStatsdOptions,
                                                  forkStatsd)
import           Text.Blaze.Html.Renderer.Text   (renderHtml)
import           Web.Scotty

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
    void $ forkStatsd defaultStatsdOptions store
    return ()
  return waiMetrics

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  waiMetrics <- handleMetrics
  port <- lookupEnv "PORT"
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
