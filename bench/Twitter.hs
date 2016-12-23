--
-- Derived from aeson examples and benchmarks
-- See https://github.com/bos/aeson
--

{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, DataKinds, ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Data (Typeable, Data)
import Data.HighJson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS

data Metadata = Metadata {
    result_type :: Text
  } deriving (Eq, Show, Typeable, Data, Generic)

metadataSpec :: JsonSpec Metadata '[Text]
metadataSpec =
    JsonSpec Metadata $
    "result_type" .= result_type
    :+: EmptySpec

instance FromJSON Metadata where
    parseJSON = makeParser metadataSpec

instance ToJSON Metadata where
    toJSON = makeSerialiser metadataSpec

instance NFData Metadata

data Metadata1 = Metadata1 {
    result_type1 :: Text
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Metadata1

data Geo = Geo {
    type_       :: Text
  , coordinates :: (Double, Double)
  } deriving (Eq, Show, Typeable, Data, Generic)

geoSpec :: JsonSpec Geo '[Text, (Double, Double)]
geoSpec =
    JsonSpec Geo $
    "type_" .= type_
    :+: "coordinates" .= coordinates
    :+: EmptySpec

instance FromJSON Geo where
    parseJSON = makeParser geoSpec

instance ToJSON Geo where
    toJSON = makeSerialiser geoSpec

instance NFData Geo

data Geo1 = Geo1 {
    type_1       :: Text
  , coordinates1 :: (Double, Double)
  } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Geo1

data Story = Story {
    from_user_id_str  :: Text
  , profile_image_url :: Text
  , created_at        :: Text
  , from_user         :: Text
  , id_str            :: Text
  , metadata          :: Metadata
  , to_user_id        :: Maybe Int64
  , text              :: Text
  , id                :: Int64
  , from_user_id      :: Int64
  , geo               :: Maybe Geo
  , iso_language_code :: Text
  , to_user_id_str    :: Maybe Text
  , source            :: Text
  } deriving (Show, Typeable, Data, Generic)

storySpec :: JsonSpec Story '[Text, Text, Text, Text, Text, Metadata, Maybe Int64, Text, Int64,
                              Int64, Maybe Geo, Text, Maybe Text, Text]
storySpec =
    JsonSpec Story $
    "from_user_id_str" .= from_user_id_str
    :+: "profile_image_url" .= profile_image_url
    :+: "created_at" .= created_at
    :+: "from_user" .= from_user
    :+: "id_str" .= id_str
    :+: "metadata" .= metadata
    :+: "to_user_id" .= to_user_id
    :+: "text" .= text
    :+: "id" .= id
    :+: "from_user_id" .= from_user_id
    :+: "geo" .= geo
    :+: "iso_language_code" .= iso_language_code
    :+: "to_user_id_str" .= to_user_id_str
    :+: "source" .= source
    :+: EmptySpec

instance FromJSON Story where
    parseJSON = makeParser storySpec

instance ToJSON Story where
    toJSON = makeSerialiser storySpec

instance NFData Story

data Story1 = Story1 {
    from_user_id_str1  :: Text
  , profile_image_url1 :: Text
  , created_at1        :: Text
  , from_user1         :: Text
  , id_str1            :: Text
  , metadata1          :: Metadata1
  , to_user_id1        :: Maybe Int64
  , text1              :: Text
  , id1                :: Int64
  , from_user_id1      :: Int64
  , geo1               :: Maybe Geo1
  , iso_language_code1 :: Text
  , to_user_id_str1    :: Maybe Text
  , source1            :: Text
  } deriving (Show, Typeable, Data, Generic)

instance NFData Story1

data Result = Result {
    results          :: [Story]
  , max_id           :: Int64
  , since_id         :: Int64
  , refresh_url      :: Text
  , next_page        :: Text
  , results_per_page :: Int
  , page             :: Int
  , completed_in     :: Double
  , since_id_str     :: Text
  , max_id_str       :: Text
  , query            :: Text
  } deriving (Show, Typeable, Data, Generic)

resultSpec :: JsonSpec Result '[[Story], Int64, Int64, Text, Text, Int, Int, Double, Text, Text,
                                Text]
resultSpec =
    JsonSpec Result $
    "results" .= results
    :+: "max_id" .= max_id
    :+: "since_id" .= since_id
    :+: "refresh_url" .= refresh_url
    :+: "next_page" .= next_page
    :+: "results_per_page" .= results_per_page
    :+: "page" .= page
    :+: "completed_in" .= completed_in
    :+: "since_id_str" .= since_id_str
    :+: "max_id_str" .= max_id_str
    :+: "query" .= query
    :+: EmptySpec

instance FromJSON Result where
    parseJSON = makeParser resultSpec

instance ToJSON Result where
    toJSON = makeSerialiser resultSpec

instance NFData Result

data Result1 = Result1 {
    results1          :: [Story1]
  , max_id1           :: Int64
  , since_id1         :: Int64
  , refresh_url1      :: Text
  , next_page1        :: Text
  , results_per_page1 :: Int
  , page1             :: Int
  , completed_in1     :: Double
  , since_id_str1     :: Text
  , max_id_str1       :: Text
  , query1            :: Text
  } deriving (Show, Typeable, Data, Generic)

instance NFData Result1

instance A.FromJSON Metadata1 where
    parseJSON =
        A.genericParseJSON $
        A.defaultOptions
        { A.fieldLabelModifier = \lbl -> take (length lbl - 1) lbl
        }
instance A.FromJSON Geo1 where
    parseJSON =
        A.genericParseJSON $
        A.defaultOptions
        { A.fieldLabelModifier = \lbl -> take (length lbl - 1) lbl
        }
instance A.FromJSON Story1 where
    parseJSON =
        A.genericParseJSON $
        A.defaultOptions
        { A.fieldLabelModifier = \lbl -> take (length lbl - 1) lbl
        }
instance A.FromJSON Result1 where
    parseJSON =
        A.genericParseJSON $
        A.defaultOptions
        { A.fieldLabelModifier = \lbl -> take (length lbl - 1) lbl
        }

checkOkIs :: Either String Result -> Result
checkOkIs lf =
    case lf of
      Left errMsg -> error $ "Benchmark failed! " ++ errMsg
      Right r -> r

checkOkIs1 :: Either String Result1 -> Result1
checkOkIs1 lf =
    case lf of
      Left errMsg -> error $ "Benchmark failed! " ++ errMsg
      Right r -> r

main :: IO ()
main =
    defaultMain
    [ env (BS.readFile "bench/json-data/twitter100.json") $ \ ~(twitter100 :: BS.ByteString) ->
          bgroup "twitter"
          [ bench "aeson/generic" $ nf (checkOkIs1 . A.eitherDecodeStrict') twitter100
          , bench "aeson/highjson" $ nf (checkOkIs . A.eitherDecodeStrict') twitter100
          ]
    , env (BS.readFile "bench/json-data/jp100.json") $ \ ~(twitter100 :: BS.ByteString) ->
          bgroup "twitter-jp"
          [ bench "aeson/generic" $ nf (checkOkIs1 . A.eitherDecodeStrict') twitter100
          , bench "aeson/highjson" $ nf (checkOkIs . A.eitherDecodeStrict') twitter100
          ]
    ]
