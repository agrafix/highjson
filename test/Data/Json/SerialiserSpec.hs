{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Json.SerialiserSpec where

import Data.HighJson.Serialiser

import Data.Aeson (encode)
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

instance ToJSON SomeDummy where
    toJSON =
        runSerSpec $ SingleConstr ("int" .: sd_int :&&&: "bool" .: sd_bool
                                   :&&&: "text" .: sd_text
                                   :&&&: "maybe" .:? sd_maybe :&&&: SerObjSpecNil)

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

instance ToJSON SomeNested where
    toJSON =
        runSerSpec $ SingleConstr ("list" .: sn_list :&&&: "obj" .:? sn_obj
                                   :&&&: SerObjSpecNil)

data Foo
   = Foo
    { f_fooVal :: Int
    } deriving (Show, Eq)

instance ToJSON Foo where
    toJSON =
        runSerSpec $ SingleConstr ("value" .: f_fooVal :&&&: SerObjSpecNil)

data Bar
   = Bar
    { b_barVal :: Int
    } deriving (Show, Eq)

instance ToJSON Bar where
    toJSON =
        runSerSpec $ SingleConstr ("value" .: b_barVal :&&&: SerObjSpecNil)

data SumType
   = SumFoo Foo
   | SumBar Bar
   deriving (Show, Eq)

instance ToJSON SumType where
    toJSON =
        runSerSpec $ MultiConstr $ \v ->
            case v of
              SumFoo f -> "foo" .<- f
              SumBar b -> "bar" .<- b

spec :: Spec
spec =
    describe "Serialiser" $
    do it "Handles custom types correctly" $
            do encode (SomeDummy 34 True "Teext" Nothing)
                                   `shouldBe` "{\"text\":\"Teext\",\"int\":34,\"bool\":true}"

               encode (SomeDummy 34 True "Teext" (Just 42))
                               `shouldBe` "{\"maybe\":42,\"text\":\"Teext\",\"int\":34,\"bool\":true}"
       it "Handles nested types correctly" $
           encode (SomeNested [SomeNested [] Nothing] (Just $ SomeNested [] Nothing))
               `shouldBe` "{\"obj\":{\"list\":[]},\"list\":[{\"list\":[]}]}"
       it "Handles sum types correctly" $
           do encode (SumFoo (Foo 42)) `shouldBe` "{\"foo\":{\"value\":42}}"
              encode (SumBar (Bar 42)) `shouldBe`  "{\"bar\":{\"value\":42}}"
