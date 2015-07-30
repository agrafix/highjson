{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Json.SerialiserSpec where

import Data.Json.Serialiser

import Data.Typeable
import Test.Hspec
import qualified Data.Text as T

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
        runSerSpec $ SingleConstr ("int" .: sd_int :&&&: "bool" .: sd_bool
                                   :&&&: "text" .: sd_text :&&&: "either" .: sd_either
                                   :&&&: "maybe" .:? sd_maybe :&&&: SerObjSpecNil)

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

instance ToJson SomeNested where
    toJson =
        runSerSpec $ SingleConstr ("list" .: sn_list :&&&: "obj" .:? sn_obj
                                   :&&&: SerObjSpecNil)

data Foo
   = Foo
    { f_fooVal :: Int
    } deriving (Show, Eq)

instance ToJson Foo where
    toJson =
        runSerSpec $ SingleConstr ("value" .: f_fooVal :&&&: SerObjSpecNil)

data Bar
   = Bar
    { b_barVal :: Int
    } deriving (Show, Eq)

instance ToJson Bar where
    toJson =
        runSerSpec $ SingleConstr ("value" .: b_barVal :&&&: SerObjSpecNil)

data SumType
   = SumFoo Foo
   | SumBar Bar
   deriving (Show, Eq)

instance ToJson SumType where
    toJson =
        runSerSpec $ MultiConstr $ \v ->
            case v of
              SumFoo f -> "foo" .<- f
              SumBar b -> "bar" .<- b

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
       it "Handles sum types correctly" $
           do serialiseJsonBs (SumFoo (Foo 42)) `shouldBe` "{\"foo\":{\"value\":42}}"
              serialiseJsonBs (SumBar (Bar 42)) `shouldBe`  "{\"bar\":{\"value\":42}}"
