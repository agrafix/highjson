{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Json.ParserSpec where

import Data.Json.Parser

import Control.Applicative
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
        runParseSpec $ SomeDummy :$: "int" :&&: "bool" :&&: "text" :&&: "either" :&&: "maybe" :&&: ObjSpecNil

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

instance JsonReadable SomeNested where
    readJson =
        runParseSpec $ SomeNested :$: "list" :&&: "obj" :&&: ObjSpecNil

data Foo
   = Foo
    { f_fooVal :: Int
    } deriving (Show, Eq)

instance JsonReadable Foo where
    readJson =
        runParseSpec $ Foo :$: "value" :&&: ObjSpecNil

data Bar
   = Bar
    { b_barVal :: Int
    } deriving (Show, Eq)

instance JsonReadable Bar where
    readJson =
        runParseSpec $ Bar :$: "value" :&&: ObjSpecNil

data SumType
   = SumFoo Foo
   | SumBar Bar
   deriving (Show, Eq)

instance JsonReadable SumType where
    readJson =
        runParseSpec $
        "foo" .-> (SumFoo <$> readJson)
        <||> "bar" .-> (SumBar <$> readJson)

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
       it "Parses strings correctly" $
            do parseJsonBs "\"Hello world\"" `shouldBe` Right ("Hello world" :: T.Text)
               parseJsonBs "\"Hello \\\"world\\\"\"" `shouldBe` Right ("Hello \"world\"" :: T.Text)
               parseJsonBs "\"Hello \\nworld\"" `shouldBe` Right ("Hello \nworld" :: T.Text)
               parseJsonBs "\"\\u0041 Hello \\\"world\\\"\"" `shouldBe` Right ("A Hello \"world\"" :: T.Text)
               parseJsonBs "\"\\u306e Hello \\\"world\\\"\"" `shouldBe` Right ("\12398 Hello \"world\"" :: T.Text)
               parseJsonBs "\"\\uD834\\uDD1E\"" `shouldBe` Right ("\RS" :: T.Text)
       it "Handles sum types correctly" $
           do parseJsonBs "{\"foo\": {\"value\": 42}}" `shouldBe` Right (SumFoo (Foo 42))
              parseJsonBs "{\"bar\": {\"value\": 42}}" `shouldBe` Right (SumBar (Bar 42))
