{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.JsonSpec where

import Data.HighJson

import Control.Lens.TH
import Data.Aeson (eitherDecode, encode)
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

someDummySpec :: RecordTypeSpec SomeDummy '[Int, Bool, T.Text, Either Bool T.Text, Maybe Int]
someDummySpec =
    recSpec "Some Dummy" Nothing SomeDummy $
    "int" .= sd_int
    :& "bool" .= sd_bool
    :& "text" .= sd_text
    :& "either" .= sd_either
    :& "maybe" .= sd_maybe

instance ToJSON SomeDummy where
    toJSON = jsonSerializer someDummySpec
    toEncoding = jsonEncoder someDummySpec

instance FromJSON SomeDummy where
    parseJSON = jsonParser someDummySpec

newtype SomeText = SomeText { unSomeText :: T.Text }

instance Arbitrary SomeText where
    arbitrary = SomeText . T.pack <$> listOf1 (choose ('A', 'Z'))

instance Arbitrary SomeDummy where
    arbitrary =
        SomeDummy
        <$> arbitrary
        <*> arbitrary
        <*> (unSomeText <$> arbitrary)
        <*> ebt
        <*> arbitrary
        where
          ebt =
              do v <- arbitrary
                 case v of
                   Left b -> pure (Left b)
                   Right (SomeText t) -> pure (Right t)

data SomeNested
   = SomeNested
   { sn_list :: [SomeNested]
   , sn_obj :: Maybe SomeNested
   } deriving (Show, Eq, Typeable)

someNestedSpec :: RecordTypeSpec SomeNested '[[SomeNested], Maybe SomeNested]
someNestedSpec =
    recSpec "Nested" Nothing SomeNested $
    "list" .= sn_list
    :& "obj" .=? sn_obj

instance ToJSON SomeNested where
    toJSON = jsonSerializer someNestedSpec
    toEncoding = jsonEncoder someNestedSpec

instance FromJSON SomeNested where
    parseJSON = jsonParser someNestedSpec

data SomeSumType
   = SomeDummyT SomeDummy
   | SomeInt Int
   | SomeBool Bool
   | SomeEnum
   deriving (Show, Eq, Typeable)

makePrisms ''SomeSumType

someSumSpec :: SumTypeSpec SomeSumType '[SomeDummy, Int, Bool, ()]
someSumSpec =
    sumSpec "SomeSum" Nothing $
    "some_dummy" .-> _SomeDummyT
    :& "some_int" .-> _SomeInt
    :& "some_bool" .-> _SomeBool
    :& "some_enum" .-> _SomeEnum

instance ToJSON SomeSumType where
    toJSON = jsonSerializer someSumSpec
    toEncoding = jsonEncoder someSumSpec

instance FromJSON SomeSumType where
    parseJSON = jsonParser someSumSpec

instance Arbitrary SomeSumType where
    arbitrary =
        oneof
        [ SomeDummyT <$> arbitrary
        , SomeInt <$> arbitrary
        , SomeBool <$> arbitrary
        ]

data ParamType a
   = ParamType
   { pt_key :: a
   , pt_val :: T.Text
   } deriving (Show, Eq, Typeable)

paramTypeSpec :: FromJSON a => RecordTypeSpec (ParamType a) '[a, T.Text]
paramTypeSpec =
    recSpec "Some Param" Nothing ParamType $
    "key" .= pt_key
    :& "val" .= pt_val

instance (ToJSON a, FromJSON a) => ToJSON (ParamType a) where
    toJSON = jsonSerializer paramTypeSpec
    toEncoding = jsonEncoder paramTypeSpec

instance (FromJSON a) => FromJSON (ParamType a) where
    parseJSON = jsonParser paramTypeSpec

instance Arbitrary a => Arbitrary (ParamType a) where
    arbitrary =
        ParamType <$> arbitrary <*> (unSomeText <$> arbitrary)

spec :: Spec
spec =
    describe "Parser and Serialiser" $
    do it "Handles custom types correctly" $
            do let t1 = SomeDummy 34 True "Teext" (Left False) Nothing
               eitherDecode (encode t1) `shouldBe` Right t1
               let t2 = SomeDummy 34 True "Teext" (Right "ok") (Just 42)
               eitherDecode (encode t2) `shouldBe` Right t2
       it "Handles arbitrary custom types correctly" $ property $ \t ->
           (eitherDecode . encode) t == Right (t :: SomeDummy)
       it "Handles arbitrary custom sum types correctly" $ property $ \t ->
           (eitherDecode . encode) t == Right (t :: SomeSumType)
       it "Handles arbitrary custom parametrized types correctly" $ property $ \t ->
           (eitherDecode . encode) t == Right (t :: ParamType SomeSumType)
       it "Handles nested types correctly" $
          do let nested = SomeNested [SomeNested [] Nothing] (Just $ SomeNested [] Nothing)
             eitherDecode (encode nested) `shouldBe` Right nested
