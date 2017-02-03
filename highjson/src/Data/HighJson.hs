{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Data.HighJson
    ( -- * A json specification for any type
      HighSpec(..)
      -- * Construct specifications for records
    , recSpec, RecordFields(..), reqField, (.=), optField, (.=?)
      -- * Construct specifications for sum types
    , sumSpec, SumOptions(..), sumOpt, (.->)
      -- * Generate json serializers/encoders and parsers from specs
    , jsonSerializer, jsonEncoder, jsonParser
      -- * Specification structures
    , BodySpec(..)
    , RecordField(..), RecordSpec(..)
    , SumOption(..), SumSpec(..)
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

(.->) :: T.Text -> Prism' t o -> SumOption t o
jsonKey .-> p = sumOpt jsonKey p

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
