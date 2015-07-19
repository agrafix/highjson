{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Json.ParserSpec where

import Data.Json.Parser

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

instance JsonReadable SomeDummy where
    readJson =
        runSpec SomeDummy $ "int" :&&: "bool" :&&: "text" :&&: "either" :&&: "maybe" :&&: ObjSpecNil

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

instance JsonReadable SomeNested where
    readJson =
        runSpec SomeNested $ "list" :&&: "obj" :&&: ObjSpecNil

spec :: Spec
spec =
    describe "Parser" $
    do it "Handles custom types correctly" $
            do parseJsonBs "{\"int\": 34, \"text\": \"Teext\", \"bool\": true, \"either\": false}"
                               `shouldBe` Right (SomeDummy 34 True "Teext" (Left False) Nothing)
               parseJsonBs "{\"int\": 34, \"text\": \"Teext\", \"bool\": true, \"either\": \"ok\", \"maybe\": 42}"
                               `shouldBe` Right (SomeDummy 34 True "Teext" (Right "ok") (Just 42))
       it "Handles extra key correctly" $
            parseJsonBs "{\"int\": 34, \"text\": \"Teext\", \"bool\": true, \"either\": false, \"extraKey\": false}"
                               `shouldBe` Right (SomeDummy 34 True "Teext" (Left False) Nothing)
       it "Handles nested types correctly" $
           do parseJsonBs "{\"list\": [{\"list\": []}], \"obj\": {\"list\": []}}"
                               `shouldBe` Right (SomeNested [SomeNested [] Nothing] (Just $ SomeNested [] Nothing))
       it "Parses bools correctly" $
            do parseJsonBs "true" `shouldBe` Right True
               parseJsonBs "false" `shouldBe` Right False
