{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Data.HighJson
    ( -- * A json specification for any type
      HighSpec(..), SpecType(..)
      -- * Construct specifications for records
    , recSpec, RecordTypeSpec, RecordFields(..), reqField, (.=), optField, (.=?)
      -- * Construct specifications for sum types
    , sumSpec, SumTypeSpec, SumOptions(..), sumOpt
      -- * Construct specifications for enum types
    , enumSpec, EnumTypeSpec, enumOpt
      -- * Shared between specifications for simplicity
    , SumMapping(..)
      -- * Generate json serializers/encoders and parsers from specs
    , jsonSerializer, jsonEncoder, jsonParser
      -- * Specification structures
    , BodySpec(..)
    , RecordField(..), RecordSpec(..)
    , SumOption(..), SumSpec(..)
    , EnumOption(..), EnumSpec(..)
      -- * Aeson reexports
    , ToJSON(..), FromJSON(..)
    )
where

import Data.HighJson.Types

import Control.Lens hiding ((.=))
import Data.Aeson ((.:), (.:?), FromJSON(..), ToJSON(..))
import qualified Data.HVect as HV
import qualified Data.Text as T

reqField :: FromJSON f => T.Text -> (t -> f) -> RecordField t f
reqField jsonKey g =
    RecordField
    { rf_jsonKey = jsonKey
    , rf_optional = False
    , rf_jsonLoader = (.:)
    , rf_get = g
    }

(.=) :: FromJSON f =>  T.Text -> (t -> f) -> RecordField t f
jsonKey .= reader = reqField jsonKey reader

optField :: FromJSON f => T.Text -> (t -> Maybe f) -> RecordField t (Maybe f)
optField jsonKey g =
    RecordField
    { rf_jsonKey = jsonKey
    , rf_optional = True
    , rf_jsonLoader = (.:?)
    , rf_get = g
    }

(.=?) :: FromJSON f =>  T.Text -> (t -> Maybe f) -> RecordField t (Maybe f)
name .=? reader = optField name reader

sumOpt :: T.Text -> Prism' t o -> SumOption t o
sumOpt jsonKey p =
    SumOption
    { so_jsonKey = jsonKey
    , so_prism = p
    }

type RecordTypeSpec t flds = HighSpec t 'SpecRecord flds

recSpec ::
    T.Text -> Maybe T.Text -> HV.HVectElim flds t
    -> RecordFields t flds
    -> RecordTypeSpec t flds
recSpec name mDesc mk fields =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecRecord $ RecordSpec (HV.uncurry mk) fields
    }

type SumTypeSpec t flds = HighSpec t 'SpecSum flds

sumSpec :: T.Text -> Maybe T.Text -> SumOptions t flds -> SumTypeSpec t flds
sumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecSum $ SumSpec opts
    }

type EnumTypeSpec t flds = HighSpec t 'SpecEnum flds

enumSpec :: T.Text -> Maybe T.Text -> [EnumOption t] -> EnumTypeSpec t flds
enumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecEnum $ EnumSpec opts
    }

enumOpt :: T.Text -> Prism' t () -> EnumOption t
enumOpt jsonKey p =
    EnumOption
    { eo_jsonKey = jsonKey
    , eo_prism = p
    }

class SumMapping x where
    type SumIn x
    type SumOut x
    (.->) :: T.Text -> Prism' (SumIn x) (SumOut x) -> x

instance SumMapping (SumOption t o) where
    type SumIn (SumOption t o) = t
    type SumOut (SumOption t o) = o
    jsonKey .-> p = sumOpt jsonKey p

instance SumMapping (EnumOption t) where
    type SumIn (EnumOption t) = t
    type SumOut (EnumOption t) = ()
    jsonKey .-> p = enumOpt jsonKey p
