{-# LANGUAGE OverloadedStrings #-}

module Views.MainPage where

import           Control.Monad                 (forM_)
import           Data.List                     (intersperse)
import qualified Data.Map                      as M
import           Data.Monoid                   (mconcat, (<>))
import           Data.Text                     (Text (..))
import           Network.Wai.Middleware.Static (static)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (toHtml, toValue, (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

import           Model                         (Lang (..), Talk (..), makeSlug)
import           Views.Layout

mainPage :: [Talk] -> H.Html
mainPage talks = mainLayout $ do
  miniBlock
  allTalks True $ filter featured talks
  cleverCloudBlock

talkBlock :: Talk -> H.Html
talkBlock talk =
  H.div ! A.class_ "section__text mdl-cell mdl-cell--10-col-desktop mdl-cell--6-col-tablet mdl-cell--3-col-phone" $ do
    H.h5 ! A.id slug $ H.toHtml $ title talk
    H.p $ H.toHtml $ description talk
    forM_ allLinks id
  where
    slug = H.toValue . makeSlug . title $ talk
    renderLinks extractor renderF = map (uncurry renderF) (M.toList $ extractor talk)
    slidesLinks = renderLinks slides renderSlidesLink
    videosLinks = renderLinks video renderVideoLink
    allLinks = intersperse (toHtml (" || " :: Text)) (slidesLinks <> videosLinks)
    renderSlidesLink En = renderLink "Read (in english 🇬🇧)"
    renderSlidesLink Fr = renderLink "Lire (en français 🇫🇷)"
    renderVideoLink En = renderLink "Watch 📹 (in english 🇬🇧)"
    renderVideoLink Fr = renderLink "Regarder 📹 (en français 🇫🇷)"
    renderLink :: Text -> Text -> H.Html
    renderLink title url =
      H.span $ H.a ! A.href (toValue url) ! A.target "_blank" $ toHtml title

cardClasses = "section--center mdl-grid mdl-grid--no-spacing mdl-shadow--2dp"

allTalks :: Bool -> [Talk] -> H.Html
allTalks isMain talks = H.section ! A.class_ cardClasses $
  H.div ! A.class_ "mdl-card mdl-cell mdl-cell--12-col" $ do
    H.div ! A.class_ "mdl-card__supporting-text mdl-grid mdl-grid--no-spacing" $ do
      H.h4 ! A.class_ "mdl-cell mdl-cell--12-col" $ "Talks"
      forM_ talks talkBlock
    if isMain then
      H.div ! A.class_ "mdl-card__actions" $
        H.a ! A.href "/talks" ! A.class_ "mdl-button" $ "All my talks"
    else
      H.div ! A.class_ "mdl-card__actions" $
        H.a ! A.href "https://www.youtube.com/playlist?list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC" ! A.class_ "mdl-button" $ "All my videos"

cleverCloudBlock :: H.Html
cleverCloudBlock = H.section ! A.class_ cardClasses $ do
  H.div ! A.class_ "mdl-card mdl-cell mdl-cell--12-col" $ do
    H.div ! A.class_"mdl-card__supporting-text" $ do
      H.h4 "Clever Cloud: The end of server management"
      "We provide IT automation to free developers from server management.\
      \Deploy to production in seconds, scale up and out automatically."
    H.div ! A.class_ "mdl-card__actions" $
      H.a ! A.href "https://clever-cloud.com" ! A.class_ "mdl-button" $ "Read more"
  H.button ! A.class_ "mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--icon" ! A.id "clever-cloud-more" $
    H.i ! A.class_ "material-icons" $ "more_vert"
  H.ul ! A.class_ "mdl-menu mdl-js-menu mdl-menu--bottom-right" ! A.for "clever-cloud-more" $ do
    menuItem "https://github.com/CleverCloud" "Github"
    menuItem "https://twitter.com/clever_cloud" "Twitter"

  where
    menuItem :: Text -> Text -> H.Html
    menuItem url name = H.li ! A.class_ "mdl-menu__item" $
      H.a ! A.href (toValue url) $ toHtml name

miniBlock :: H.Html
miniBlock = H.section ! A.class_ cardClasses $ do
  H.header ! A.class_ "section__play-btn mdl-cell mdl-cell--3-col-desktop mdl-cell--2-col-tablet mdl-cell--4-col-phone" $
    H.img ! A.src "/assets/profile2.jpg" ! A.alt "Clément Delafargue" ! A.class_ "profile-pic"
  H.div ! A.class_ "mdl-card mdl-cell mdl-cell--9-col-desktop mdl-cell--6-col-tablet mdl-cell--4-col-phone" $ do
    H.div ! A.class_ "mdl-card__supporting-text" $ do
      H.h4 "Hi, I'm Clément"
      "I'm a functional programmer, serving as CTO at Clever Cloud, where I work on IT automation \
      \and continuous delivery. I love discussing about FP, distributed systems and cloud architecture. \
      \If you've seen this page pop up in \"nearby\", let's have a chat!"
    H.div ! A.class_ "mdl-card__actions" $
      H.a ! A.href "https://twitter.com/clementd" ! A.target "_blank" ! A.class_ "mdl-button" $ "@clementd"
