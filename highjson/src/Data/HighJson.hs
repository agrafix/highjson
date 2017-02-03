{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HighJson
    ( -- * A json specification for any type
      HighSpec(..)
      -- * Construct specifications for records
    , recSpec, RecordFields(..)
      -- * Construct specifications for sum types
    , sumSpec, SumOptions(..)
      -- * Generate json serializers/encoders and parsers from specs
    , jsonSerializer, jsonEncoder, jsonParser
      -- * Specification structures
    , BodySpec(..)
    , RecordField(..), RecordSpec(..)
    , SumOption(..), SumSpec(..)
    )
where

import Data.HighJson.Types

import Control.Lens
import Control.Lens.TH
import Data.Aeson ((.:), (.:?), FromJSON, ToJSON)
import qualified Data.HVect as HV
import qualified Data.Text as T

reqField :: FromJSON f => T.Text -> T.Text -> (t -> f) -> RecordField t f
reqField name jsonKey g =
    RecordField
    { rf_name = name
    , rf_jsonKey = jsonKey
    , rf_optional = False
    , rf_jsonLoader = (.:)
    , rf_get = g
    }

optField :: FromJSON f => T.Text -> T.Text -> (t -> Maybe f) -> RecordField t (Maybe f)
optField name jsonKey g =
    RecordField
    { rf_name = name
    , rf_jsonKey = jsonKey
    , rf_optional = True
    , rf_jsonLoader = (.:?)
    , rf_get = g
    }

sumOpt :: T.Text -> T.Text -> Prism' t o -> SumOption t o
sumOpt name jsonKey prism =
    SumOption
    { so_name = name
    , so_jsonKey = jsonKey
    , so_prism = prism
    }

recSpec :: T.Text -> Maybe T.Text -> HV.HVectElim flds t -> RecordFields t flds -> HighSpec t flds
recSpec name mDesc mk fields =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec =
            BodySpecRecord $ RecordSpec (HV.uncurry mk) fields
    }

sumSpec :: T.Text -> Maybe T.Text -> SumOptions t flds -> HighSpec t flds
sumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecSum $ SumSpec opts
    }

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_eitherf :: Either Bool T.Text
   , sd_maybef :: Maybe Int
   }

makeLenses ''SomeDummy

dummySpec :: HighSpec SomeDummy _
dummySpec =
    recSpec "Some Dummy" Nothing SomeDummy $
    reqField "Int" "int" sd_int
    :+: reqField "Int" "bool" sd_bool
    :+: reqField "Int" "text" sd_text
    :+: reqField "Int" "either" sd_eitherf
    :+: optField "Int" "maybe" sd_maybef
    :+: RFEmpty

data SomeSumType
   = SomeDummyT SomeDummy
   | SomeInt Int
   | SomeBool Bool
   | SomeEnum

makePrisms ''SomeSumType

someSpec :: HighSpec SomeSumType _
someSpec =
    sumSpec "SomeSum" Nothing $
    sumOpt "SomeDummyT" "some_dummy" _SomeDummyT
    :|: sumOpt "SomeInt" "some_int" _SomeInt
    :|: sumOpt "SomeBool" "some_bool" _SomeBool
    :|: sumOpt "SomeEnum" "some_enum" _SomeEnum
    :|: SOEmpty
