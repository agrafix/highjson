{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Json.SerialiserSpec where

import Data.Json.Serialiser

import Data.Typeable
import qualified Data.Text as T
import Test.Hspec

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_either :: Either Bool T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

instance ToJson SomeDummy where
    toJson =
        runSerSpec ("int" .: sd_int :&&&: "bool" .: sd_bool
                                :&&&: "text" .: sd_text :&&&: "either" .: sd_either
                                :&&&: "maybe" .:? sd_maybe :&&&: SerSpecNil)

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

instance ToJson SomeNested where
    toJson =
        runSerSpec ("list" .: sn_list :&&&: "obj" .:? sn_obj
                                :&&&: SerSpecNil)

spec :: Spec
spec =
    describe "Serialiser" $
    do it "Handles custom types correctly" $
            do serialiseJsonBs (SomeDummy 34 True "Teext" (Left False) Nothing)
                                   `shouldBe` "{\"int\":34,\"bool\":true,\"text\":\"Teext\",\"either\":false}"

               serialiseJsonBs (SomeDummy 34 True "Teext" (Right "ok") (Just 42))
                               `shouldBe` "{\"int\":34,\"bool\":true,\"text\":\"Teext\",\"either\":\"ok\",\"maybe\":42}"
       it "Handles nested types correctly" $
           serialiseJsonBs (SomeNested [SomeNested [] Nothing] (Just $ SomeNested [] Nothing))
               `shouldBe` "{\"list\":[{\"list\":[]}],\"obj\":{\"list\":[]}}"
