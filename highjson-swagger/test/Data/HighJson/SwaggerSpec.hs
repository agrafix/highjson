{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.HighJson.SwaggerSpec
    ( spec )
where

import Data.HighJson
import Data.HighJson.Swagger

import Control.Lens.TH
import Data.Swagger
import Test.Hspec
import Test.QuickCheck hiding (Success)
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
    :& "maybe" .=? sd_maybe

instance ToJSON SomeDummy where
    toJSON = jsonSerializer someDummySpec
    toEncoding = jsonEncoder someDummySpec

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

instance ToSchema SomeDummy where
    declareNamedSchema p = makeDeclareNamedSchema someDummySpec p

data SomeSum
    = SomeSumInt Int
    | SomeSumDummy SomeDummy
    deriving (Show, Eq)

makePrisms ''SomeSum

someSumSpec :: SumTypeSpec SomeSum '[Int, SomeDummy]
someSumSpec =
    sumSpec "some sum" Nothing $
    "int" .-> _SomeSumInt
    :& "dummy" .-> _SomeSumDummy

instance ToJSON SomeSum where
    toJSON = jsonSerializer someSumSpec
    toEncoding = jsonEncoder someSumSpec

instance ToSchema SomeSum where
    declareNamedSchema p = makeDeclareNamedSchema someSumSpec p

instance Arbitrary SomeSum where
    arbitrary =
        oneof
        [ SomeSumInt <$> arbitrary
        , SomeSumDummy <$> arbitrary
        ]

data SomeEnum
    = SomeEnumA
    | SomeEnumB
    deriving (Show, Eq)

makePrisms ''SomeEnum

someEnumSpec :: EnumTypeSpec SomeEnum '[(), ()]
someEnumSpec =
    enumSpec "some enum" Nothing
    [ "int" @-> _SomeEnumA
    , "dummy" @-> _SomeEnumB
    ]

instance ToJSON SomeEnum where
    toJSON = jsonSerializer someEnumSpec
    toEncoding = jsonEncoder someEnumSpec

instance ToSchema SomeEnum where
    declareNamedSchema p = makeDeclareNamedSchema someEnumSpec p

instance Arbitrary SomeEnum where
    arbitrary =
        oneof
        [ pure SomeEnumA
        , pure SomeEnumB
        ]

spec :: Spec
spec =
    do it "should work for records" $
           property $ \(t :: SomeDummy) -> validateToJSON t `shouldBe` []
       it "should work for sum types" $
           property $ \(t :: SomeSum) -> validateToJSON t `shouldBe` []
       it "should work for enum types" $
           property $ \(t :: SomeEnum) -> validateToJSON t `shouldBe` []
