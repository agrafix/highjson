{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.JsonSpec where

import Data.Json

import Control.Applicative
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_either :: Either Bool T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

someDummySpec :: JsonSpec SomeDummy '[Int, Bool, T.Text, Either Bool T.Text, Maybe Int]
someDummySpec =
    JsonSpec SomeDummy $
    "int" .= sd_int
    :+: "bool" .= sd_bool
    :+: "text" .= sd_text
    :+: "either" .= sd_either
    :+: "maybe" .=? sd_maybe
    :+: EmptySpec

instance ToJson SomeDummy where
    toJson = makeSerialiser someDummySpec

instance JsonReadable SomeDummy where
    readJson = makeParser someDummySpec

instance Arbitrary T.Text where
    arbitrary = T.pack <$> listOf1 (choose ('A', 'Z'))

instance Arbitrary SomeDummy where
    arbitrary =
        SomeDummy
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

someNestedSpec :: JsonSpec SomeNested '[[SomeNested], Maybe SomeNested]
someNestedSpec =
    JsonSpec SomeNested $
    "list" .= sn_list
    :+: "obj" .=? sn_obj
    :+: EmptySpec

instance ToJson SomeNested where
    toJson = makeSerialiser someNestedSpec

instance JsonReadable SomeNested where
    readJson = makeParser someNestedSpec

data SomeSumType
   = SomeDummyT SomeDummy
   | SomeInt Int
   | SomeBool Bool
   deriving (Show, Eq)

someSumSpec :: JsonSumSpec SomeSumType
someSumSpec =
    JsonSumSpec
    { js_parser =
            "dummy" .-> (SomeDummyT <$> readJson)
       <||> "int" .-> (SomeInt <$> readJson)
       <||> "bool" .-> (SomeBool <$> readJson)
    , js_serialiser =
          \v ->
              case v of
                SomeDummyT d -> "dummy" .<- d
                SomeInt i -> "int" .<- i
                SomeBool b -> "bool" .<- b
    }

instance ToJson SomeSumType where
    toJson = makeSumSerialiser someSumSpec

instance JsonReadable SomeSumType where
    readJson = makeSumParser someSumSpec

instance Arbitrary SomeSumType where
    arbitrary =
        oneof
        [ SomeDummyT <$> arbitrary
        , SomeInt <$> arbitrary
        , SomeBool <$> arbitrary
        ]

spec :: Spec
spec =
    describe "Parser and Serialiser" $
    do it "Handles custom types correctly" $
            do let t1 = SomeDummy 34 True "Teext" (Left False) Nothing
               parseJsonBs (serialiseJsonBs t1) `shouldBe` Right t1
               let t2 = SomeDummy 34 True "Teext" (Right "ok") (Just 42)
               parseJsonBs (serialiseJsonBs t2) `shouldBe` Right t2
       it "Handles arbitrary custom types correctly" $ property $ \t ->
           (parseJsonBs . serialiseJsonBs) t == Right (t :: SomeDummy)
       it "Handles arbitrary custom sum types correctly" $ property $ \t ->
           (parseJsonBs . serialiseJsonBs) t == Right (t :: SomeSumType)
       it "Handles nested types correctly" $
          do let nested = SomeNested [SomeNested [] Nothing] (Just $ SomeNested [] Nothing)
             parseJsonBs (serialiseJsonBs nested) `shouldBe` Right nested
