{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Views.MainPage where

import           Data.Foldable               (sequenceA_, traverse_)
import           Data.List                   (intersperse)
import           Data.List.NonEmpty          (NonEmpty, toList)
import qualified Data.Map                    as M
import           Data.Text                   (Text)
import           Text.Blaze.Html5            (toHtml, toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Model                       (Item, Lang (..), Project (..),
                                              Talk (..), makeSlug)
import           Views.Layout

mainPage :: [Talk] -> [Project] -> H.Html
mainPage talks projects = mainLayout $ do
  miniBlock
  allTalks True $ filter featured talks
  openSource projects
  companyBlock

talkBlock :: Talk -> H.Html
talkBlock talk@Talk{title,description}=
  H.div ! A.class_ "section__text mdl-cell mdl-cell--10-col-desktop mdl-cell--6-col-tablet mdl-cell--3-col-phone" ! A.id (H.toValue slug) $ do
    H.h5 $
      H.a ! A.class_ "permalink" ! A.href (H.toValue $ "/me/talks#" <> slug) $
        H.toHtml $ title
    H.p $ H.toHtml $ description
    sequenceA_ allLinks
  where
    slug = makeSlug title
    renderLinks extractor renderF = foldMap (uncurry renderF) (M.toList $ extractor talk)
    slidesLinks = renderLinks slides renderSlidesLinks
    videosLinks = renderLinks video renderVideoLinks
    allLinks = intersperse (toHtml (" || " :: Text)) (slidesLinks <> videosLinks)
    renderSlidesLinks :: Lang -> NonEmpty Item -> [H.Html]
    renderSlidesLinks En = toList . fmap (renderLink $ mkSlidesTitle En)
    renderSlidesLinks Fr = toList . fmap (renderLink $ mkSlidesTitle Fr)
    renderVideoLinks :: Lang -> NonEmpty Item -> [H.Html]
    renderVideoLinks En = toList . fmap (renderLink $ mkVideoTitle En)
    renderVideoLinks Fr = toList . fmap (renderLink $ mkVideoTitle Fr)
    mkVideoTitle :: Lang -> Maybe Text -> Text
    mkVideoTitle En (Just label) = "Watch 📹 (in english 🇬🇧, at " <> label <> ")"
    mkVideoTitle En _            = "Watch 📹 (in english 🇬🇧)"
    mkVideoTitle Fr (Just label) = "Regarder 📹 (en français 🇫🇷, à " <> label <> ")"
    mkVideoTitle Fr _            = "Regarder 📹 (en français 🇫🇷)"
    mkSlidesTitle :: Lang -> Maybe Text -> Text
    mkSlidesTitle En (Just label) = "Read (in english 🇬🇧, at " <> label <> ")"
    mkSlidesTitle En _            = "Read (in english 🇬🇧)"
    mkSlidesTitle Fr (Just label) = "Lire (en français 🇫🇷, à " <> label <> ")"
    mkSlidesTitle Fr _            = "Lire (en français 🇫🇷)"
    renderLink :: (Maybe Text -> Text) -> Item -> H.Html
    renderLink mkTitle (label, url) =
      H.span $
        H.a ! A.href (toValue url)
            ! A.target "_blank"
            ! A.rel "noopener"
            $ toHtml (mkTitle label)

cardClasses :: H.AttributeValue
cardClasses = "section--center mdl-grid mdl-grid--no-spacing mdl-shadow--2dp"

allTalks :: Bool -> [Talk] -> H.Html
allTalks isMain talks = H.section ! A.class_ cardClasses $
  H.div ! A.class_ "mdl-card mdl-cell mdl-cell--12-col" $ do
    H.div ! A.class_ "mdl-card__supporting-text mdl-grid mdl-grid--no-spacing" $ do
      H.h4 ! A.class_ "mdl-cell mdl-cell--12-col" $ "Talks"
      traverse_ talkBlock talks
    if isMain then
      H.div ! A.class_ "mdl-card__actions" $
        H.a ! A.href "/me/talks" ! A.class_ "mdl-button" $ "All my talks"
    else
      H.div ! A.class_ "mdl-card__actions" $
        H.a ! A.href "https://www.youtube.com/playlist?list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC" ! A.class_ "mdl-button" $ "All my videos"

projectBlock :: Project -> H.Html
projectBlock Project{title,description, url}=
  H.div ! A.class_ "section__text mdl-cell mdl-cell--10-col-desktop mdl-cell--6-col-tablet mdl-cell--3-col-phone" $ do
    H.h5 $
      H.toHtml $ title
    H.p $ H.toHtml $ description
    H.span $
      H.a ! A.href (toValue url)
          ! A.target "_blank"
          ! A.rel "noopener"
          $ toHtml url

openSource :: [Project] -> H.Html
openSource projects = H.section ! A.class_ cardClasses $
  H.div ! A.class_ "mdl-card mdl-cell mdl-cell--12-col" $ do
    H.div ! A.class_ "mdl-card__supporting-text mdl-grid mdl-grid--no-spacing" $ do
      H.h4 ! A.class_ "mdl-cell mdl-cell--12-col" $ "Open-source"
      traverse_ projectBlock projects

companyBlock :: H.Html
companyBlock = H.section ! A.class_ cardClasses $ do
  H.div ! A.class_ "mdl-card mdl-cell mdl-cell--12-col" $ do
    H.div ! A.class_"mdl-card__supporting-text" $ do
      H.h4 "FretLink: Shipment made simpler"
      "FretLink is a flow management solution to industrial shippers that brings \
      \more visibility to all stakeholders in the chain."
    H.div ! A.class_ "mdl-card__actions" $
      H.a ! A.href "https://fretlink.com" ! A.class_ "mdl-button" $ "Read more"
  H.button ! A.class_ "mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--icon" ! A.id "fretlink-more" $
    H.i ! A.class_ "material-icons" $ "more_vert"
  H.ul ! A.class_ "mdl-menu mdl-js-menu mdl-menu--bottom-right" ! A.for "fretlink-more" $ do
    menuItem "https://github.com/fretlink" "Github"
    menuItem "https://twitter.com/fretlinkeu" "Twitter"
  where
    menuItem :: Text -> Text -> H.Html
    menuItem url name = H.li ! A.class_ "mdl-menu__item" $
      H.a ! A.href (toValue url) $ toHtml name

miniBlock :: H.Html
miniBlock = H.section ! A.class_ cardClasses $ do
  H.header ! A.class_ "section__play-btn mdl-cell mdl-cell--3-col-desktop mdl-cell--2-col-tablet mdl-cell--4-col-phone" $
    H.img ! A.src "/me/assets/profile2.jpg" ! A.alt "Clément Delafargue" ! A.class_ "profile-pic"
  H.div ! A.class_ "mdl-card mdl-cell mdl-cell--9-col-desktop mdl-cell--6-col-tablet mdl-cell--4-col-phone" $ do
    H.div ! A.class_ "mdl-card__supporting-text" $ do
      H.h4 "Hi, I'm Clément"
      "I'm a functional programmer working at FretLink. I love discussing about FP, distributed systems and cloud architecture. \
      \If you've seen this page pop up in \"nearby\", let's have a chat!"
    H.div ! A.class_ "mdl-card__actions" $
      H.a ! A.href "https://twitter.com/clementd" ! A.target "_blank" ! A.class_ "mdl-button" $ "@clementd"
