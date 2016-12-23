{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Json.ParserSpec where

import Data.HighJson.Parser

import Data.Aeson (eitherDecode)
import Data.Typeable
import Test.Hspec
import qualified Data.Text as T

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

instance FromJSON SomeDummy where
    parseJSON =
        runParseSpec $
        SomeDummy
        :$: reqKey "int"
        :&&: reqKey"bool"
        :&&: reqKey"text"
        :&&: optKey "maybe"
        :&&: ObjSpecNil

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

instance FromJSON SomeNested where
    parseJSON =
        runParseSpec $ SomeNested :$: reqKey "list" :&&: optKey "obj" :&&: ObjSpecNil

data Foo
   = Foo
    { f_fooVal :: Int
    } deriving (Show, Eq)

instance FromJSON Foo where
    parseJSON =
        runParseSpec $ Foo :$: reqKey "value" :&&: ObjSpecNil

data Bar
   = Bar
    { b_barVal :: Int
    } deriving (Show, Eq)

instance FromJSON Bar where
    parseJSON =
        runParseSpec $ Bar :$: reqKey "value" :&&: ObjSpecNil

data SumType
   = SumFoo Foo
   | SumBar Bar
   deriving (Show, Eq)

instance FromJSON SumType where
    parseJSON =
        runParseSpec $
        "foo" .-> SumFoo
        <||> "bar" .-> SumBar

data ParamType k
   = ParamType
   { pt_key :: k
   , pt_val :: T.Text
   } deriving (Show, Eq)

instance (Typeable k, FromJSON k) => FromJSON (ParamType k) where
    parseJSON =
        runParseSpec $ ParamType :$: reqKey "key" :&&: reqKey "val" :&&: ObjSpecNil

spec :: Spec
spec =
    describe "Parser" $
    do it "Handles custom types correctly" $
            do eitherDecode "{\"int\": 34, \"text\": \"Teext\", \"bool\": true}"
                               `shouldBe` Right (SomeDummy 34 True "Teext" Nothing)
               eitherDecode "{\"int\": 34, \"text\": \"Teext\", \"bool\": true, \"maybe\": 42}"
                               `shouldBe` Right (SomeDummy 34 True "Teext" (Just 42))
       it "Handles extra key correctly" $
            eitherDecode "{\"int\": 34, \"text\": \"Teext\", \"bool\": true, \"extraKey\": false}"
                               `shouldBe` Right (SomeDummy 34 True "Teext" Nothing)
       it "Handles nested types correctly" $
           eitherDecode "{\"list\": [{\"list\": []}], \"obj\": {\"list\": []}}"
                           `shouldBe` Right (SomeNested [SomeNested [] Nothing] (Just $ SomeNested [] Nothing))
       it "Handles parametrized types correctly" $
           eitherDecode "{\"key\": true, \"val\": \"hi\"}"
                           `shouldBe` Right (ParamType True "hi")
       it "Parses bools correctly" $
            do eitherDecode "true" `shouldBe` Right True
               eitherDecode "false" `shouldBe` Right False
       it "Parses strings correctly" $
            do eitherDecode "\"Hello world\"" `shouldBe` Right ("Hello world" :: T.Text)
               eitherDecode "\"Hello \\\"world\\\"\"" `shouldBe` Right ("Hello \"world\"" :: T.Text)
               eitherDecode "\"Hello \\nworld\"" `shouldBe` Right ("Hello \nworld" :: T.Text)
               eitherDecode "\"\\u0041 Hello \\\"world\\\"\"" `shouldBe` Right ("A Hello \"world\"" :: T.Text)
               eitherDecode "\"\\u306e Hello \\\"world\\\"\"" `shouldBe` Right ("\12398 Hello \"world\"" :: T.Text)
       it "Handles sum types correctly" $
           do eitherDecode "{\"foo\": {\"value\": 42}}" `shouldBe` Right (SumFoo (Foo 42))
              eitherDecode "{\"bar\": {\"value\": 42}}" `shouldBe` Right (SumBar (Bar 42))
