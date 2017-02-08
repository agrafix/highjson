{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.HighJson.THSpec
    ( spec )
where

import Data.HighJson
import Data.HighJson.TH

import Control.Lens.TH
import Data.Swagger
import Test.Hspec
import Test.QuickCheck hiding (Success)

data SomeSum
    = SomeSumInt Int
    | SomeSumDummy Bool
    deriving (Show, Eq)

makePrisms ''SomeSum

someSumSpec :: SumTypeSpec SomeSum '[Int, Bool]
someSumSpec =
    sumSpec "some sum" Nothing $
    "int" .-> _SomeSumInt
    :& "dummy" .-> _SomeSumDummy

$(deriveJsonSwagger ''SomeSum 'someSumSpec)

instance Arbitrary SomeSum where
    arbitrary =
        oneof
        [ SomeSumInt <$> arbitrary
        , SomeSumDummy <$> arbitrary
        ]

spec :: Spec
spec =
    it "should work for sum types" $
    property $ \(t :: SomeSum) -> validateToJSON t `shouldBe` []
