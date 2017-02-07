{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HighJson.SwaggerSpec
    ( spec )
where

import Data.HighJson
import Data.HighJson.Swagger

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
    :+: "bool" .= sd_bool
    :+: "text" .= sd_text
    :+: "either" .= sd_either
    :+: "maybe" .=? sd_maybe
    :+: RFEmpty

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

spec :: Spec
spec =
    it "schema and serializer match" $ property $ \(t :: SomeDummy) ->
    validateToJSON t `shouldBe` []
