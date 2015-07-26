--
-- Derived from aeson examples and benchmarks
-- See https://github.com/bos/aeson
--

{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables #-}

module Main where

import Data.Data (Typeable, Data)
import Data.Int (Int64)
import Data.Text (Text)
import Criterion.Main
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Prelude hiding (id)
import Control.DeepSeq
import Data.Json.Parser
import qualified Data.ByteString as BS

data Metadata = Metadata {
    result_type :: Text
  } deriving (Eq, Show, Typeable, Data, Generic)

instance JsonReadable Metadata where
    readJson =
        runParseSpec $ Metadata :$: "result_type" :&&: ObjSpecNil

instance NFData Metadata

data Geo = Geo {
    type_       :: Text
  , coordinates :: (Double, Double)
  } deriving (Eq, Show, Typeable, Data, Generic)

instance JsonReadable Geo where
    readJson =
        runParseSpec $ Geo :$: "type_" :&&: "coordinates" :&&: ObjSpecNil

instance NFData Geo

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

instance JsonReadable Story where
    readJson =
        runParseSpec $ Story :$:
            "from_user_id_str" :&&: "profile_image_url" :&&: "created_at"
              :&&: "from_user" :&&: "id_str" :&&: "metadata" :&&: "to_user_id"
              :&&: "text" :&&: "id" :&&: "from_user_id" :&&: "geo" :&&: "iso_language_code"
              :&&: "to_user_id_str" :&&: "source"
              :&&: ObjSpecNil

instance NFData Story

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

instance JsonReadable Result where
    readJson =
        runParseSpec $ Result :$:
            "results" :&&: "max_id" :&&: "since_id" :&&: "refresh_url" :&&: "next_page"
              :&&: "results_per_page" :&&: "page" :&&: "completed_in" :&&: "since_id_str"
              :&&: "max_id_str" :&&: "query"
              :&&: ObjSpecNil

instance NFData Result

instance A.FromJSON Metadata
instance A.FromJSON Geo
instance A.FromJSON Story
instance A.FromJSON Result

checkOkIs :: Either String Result -> Result
checkOkIs lf =
    case lf of
      Left errMsg -> error $ "Benchmark failed! " ++ errMsg
      Right r -> r

main :: IO ()
main =
    defaultMain
    [ env (BS.readFile "bench/json-data/twitter100.json") $ \ ~(twitter100 :: BS.ByteString) ->
          bgroup "twitter"
          [ bench "aeson" $ nf (checkOkIs . A.eitherDecodeStrict') twitter100
          , bench "highjson" $ nf (checkOkIs . parseJsonBs) twitter100
          ]
    ]
