{-# LANGUAGE OverloadedStrings #-}

module Views.Layout where

import Control.Monad (forM_)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text(..))
import Network.Wai.Middleware.Static (static)
import Text.Blaze.Html5 ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model (Talk(..))

mainLayout :: H.Html -> H.Html
mainLayout content = H.docTypeHtml $ do
  headBlock
  bodyBlock headerBlock $ mainBlock content

headBlock :: H.Html
headBlock = H.head $ do
  H.meta ! A.charset "utf-8"
  H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
  H.title " 🌈 Clément Delafargue  🌈 "
  forM_ meta (\(name, content) -> H.meta ! A.name (toValue name) ! A.content (toValue content))
  --I don't have icons yet
  --H.link ! A.rel "icon" ! A.sizes "192x192" ! A.href "images/android-desktop.png"
  --H.link ! A.rel "apple-touch-icon-precomposed" ! A.href "images/ios-desktop.png"
  --H.link ! A.rel "shortcut icon" ! A.href "images/favicon.png"
  forM_ stylesheets (\href -> H.link ! A.rel "stylesheet" ! A.href (toValue href))

  where
    meta :: [(Text, Text)]
    meta =
      [ ("description",                  "Clément Delafargue. Functional Programming and Continuous Deployment")
      , ("viewport",                     "width=device-width, initial-scale=1.0, minimum-scale=1.0")
      , ("mobile-web-app-capable",       "yes")
      , ("apple-mobile-web-app-capable", "yes")
      , ("apple-mobile-web-app-status-bar-style", "black")
      , ("apple-mobile-web-app-title",   "Clément Delafargue")
      --, ("msapplication-TileImage",      "images/touch/ms-touch-icon-144x144-precomposed.png")
      , ("msapplication-TileColor",      "#3372DF")
      ]

    stylesheets :: [Text]
    stylesheets =
      [ "https://fonts.googleapis.com/css?family=Roboto:regular,bold,italic,thin,light,bolditalic,black,medium&amp;lang=en"
      , "https://fonts.googleapis.com/icon?family=Material+Icons"
      , "assets/material.orange-deep_orange.min.css"
      , "assets/style.css"
      ]

bodyBlock :: H.Html -> H.Html -> H.Html
bodyBlock header content = H.body ! A.class_ "mdl-demo mdl-color--grey-100 mdl-color-text--grey-700 mdl-base" $ do
  H.div ! A.class_ "mdl-layout mdl-js-layout mdl-layout--fixed-header" $ do
    header
    content
  H.script ! A.src "assets/material.min.js" $ ""

headerBlock :: H.Html
headerBlock = H.header ! A.class_ "mdl-layout__header mdl-layout__header--scroll mdl-color--primary" $
   H.div ! A.class_ "mdl-layout__header-row" $ H.h1 "Clément Delafargue"

mainBlock :: H.Html -> H.Html
mainBlock content = H.main ! A.class_ "mdl-layout__content" $ do
  content
  footerBlock

footerBlock :: H.Html
footerBlock = H.footer ! A.class_ "mdl-mega-footer" $
   H.div ! A.class_ "mdl-mega-footer--bottom-section" $
      H.ul ! A.class_ "mdl-mega-footer--link-list" $ do
        H.li $ H.a ! A.href "#" $ "About me"
        H.li $ H.a ! A.href "https://twitter.com/clementd" $ "Twitter"
        H.li $ H.a ! A.href "https://github.com/divarvel" $ "Github"
        H.li $ H.a ! A.href "http://blog.clement.delafargue.name" $ "Tech blog"
