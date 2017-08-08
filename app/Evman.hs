{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}


module Evman where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.List
import qualified Data.Map             as M
import           Data.Text            (Text (..))
import qualified Data.Text            as T
import           GHC.Generics
import           Network.Wreq

import           Model                (Lang (..), Talk (..))

data EvmanResult = EvmanResult
    { user :: EvmanUser
    } deriving (Generic)

data EvmanUser = EvmanUser
    { talks :: [EvmanTalk]
    } deriving (Generic)

data EvmanTalk = EvmanTalk
    { id             :: Integer
    , name           :: Text
    , abstract       :: Text
    , resource_links :: [EvmanResource]
    } deriving (Generic)

data EvmanResource = EvmanResource
    { name             :: Text
    , link             :: Text
    , resource_type_id :: Integer
    } deriving (Generic)

instance FromJSON EvmanResult
instance FromJSON EvmanUser
instance FromJSON EvmanTalk
instance FromJSON EvmanResource

decodeResult :: ByteString -> Maybe EvmanResult
decodeResult = decode

toLink :: EvmanResource -> (Lang, Text)
toLink r = (lang, url)
  where
    nn :: EvmanResource -> Text
    nn = name
    lang = if nn r == "(in english 🇬🇧)" then En else Fr
    url = link r

toLinks :: Integer -> [EvmanResource] -> M.Map Lang Text
toLinks lType rs = let
    resources = filter ((== lType) . resource_type_id) rs
    in M.fromList . map toLink $ resources

featuredIds :: [Integer]
featuredIds = [31, 34]

hiddenIds :: [Integer]
hiddenIds = [33, 38, 39, 40, 41]

removeHidden :: EvmanTalk -> Bool
removeHidden (EvmanTalk i _ _ _) = not $ i `elem` hiddenIds

toTalk :: EvmanTalk -> Talk
toTalk (EvmanTalk i n a rls) = Talk n (head . T.lines $ a) slides videos feat
  where
    slides = toLinks 2 rls
    videos = toLinks 1 rls
    feat = i `elem` featuredIds

resultToTalks :: EvmanResult -> [Talk]
resultToTalks r = sortOn title ts
  where
    ts = fmap toTalk (filter removeHidden . talks . user $ r)

loadTalks :: IO [Talk]
loadTalks = do
    r <- get "https://evman.clever-cloud.com/public/user/4.json"
    let body = r ^. responseBody
    case decodeResult body of
        (Just result) -> return $ resultToTalks result
        _             -> return []

