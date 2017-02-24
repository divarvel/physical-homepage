{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model (Talk(..))
import Views.MainPage

talks =
  [ Talk
      "Acknowledging boundaries"
      "Get consistency back in your μServices architecture: \
        \the best way to reduce complexity in a μServices architecture is to embrace boundaries. \
        \You'll see how to do it with the help of proper design and a good type system."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/uploads/acknowledging-boundaries.html")
      True
  , Talk
      "TDD as in Type-Driven Development"
      "Test-Driven Development is widely accepted as good practice. But can we do better? \
      \By specifying your program's behaviour with types, you can go a very long way, \
      \with more confidence and with less hassle than with tests."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/type-dd-devoxxfr.html")
      False
  , Talk
      "Functional patterns for scala practitioners"
      "Scala, by being both Functional and Object-Oriented is easy to get started with, especially for java developpers. \
      \However, to get the most of the language, you have to embrace its functional nature."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/fp-patterns-devoxx-be.html")
      True
  ]

main = scotty 8080 $ do
  middleware $ staticPolicy (hasPrefix  "assets/")
  get "/" $
    html $ renderHtml $ mainPage talks
