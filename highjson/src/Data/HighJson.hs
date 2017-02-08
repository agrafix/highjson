{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
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
    , sumSpec, SumTypeSpec, SumOptions(..), sumOpt, (.->)
      -- * Construct specifications for enum types
    , enumSpec, EnumTypeSpec, enumOpt, (@->)
      -- * Shared between specifications for simplicity
    , IsDataSpec(..), (:&)(..), CombinableContainer(..)
      -- * Generate json serializers/encoders and parsers from specs
    , jsonSerializer, jsonEncoder, jsonParser
      -- * Specification structures
    , BodySpec(..)
    , RecordField(..), RecordSpec(..)
    , SumOption(..), SumSpec(..)
    , EnumOption(..), EnumSpec(..)
      -- * Aeson reexports
    , ToJSON(..), FromJSON(..)
      -- * Implementation detail structures
    , PhantomEnumContainer(..)
    )
where

import Data.HighJson.Types

import Control.Lens hiding ((.=))
import Data.Aeson ((.:), (.:?), FromJSON(..), ToJSON(..))
import Data.Typeable
import qualified Data.HVect as HV
import qualified Data.Text as T

data a :& b
    = a :& b
    deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 8 :&

instance (Monoid a, Monoid b) => Monoid (a :& b) where
    mempty = mempty :& mempty
    (a :& b) `mappend` (a' :& b') = (a `mappend` a') :& (b `mappend` b')

class CombinableContainer t where
    combineContainer :: t a (as :: [*]) -> t a (bs :: [*]) -> t a (HV.Append as bs)

instance CombinableContainer RecordFields where
    combineContainer = recAppend

instance CombinableContainer SumOptions where
    combineContainer = sumAppend

instance CombinableContainer PhantomEnumContainer where
    combineContainer (PhantomEnumContainer x) (PhantomEnumContainer y) =
        PhantomEnumContainer $ x ++ y

class IsDataSpec t where
    type DFields t :: [*]
    type DType t
    type DContainer t :: * -> [*] -> *
    compileRec :: t -> (DContainer t) (DType t) (DFields t)

instance IsDataSpec (RecordField t f) where
    type DFields (RecordField t f) = (f ': '[])
    type DType (RecordField t f) = t
    type DContainer (RecordField t f) = RecordFields
    compileRec x = x :+: RFEmpty

instance IsDataSpec (SumOption t f) where
    type DFields (SumOption t f) = (f ': '[])
    type DType (SumOption t f) = t
    type DContainer (SumOption t f) = SumOptions
    compileRec x = x :|: SOEmpty

newtype PhantomEnumContainer t (ts :: [*])
    = PhantomEnumContainer { unPhantomEnumContainer :: [EnumOption t] }

instance IsDataSpec (EnumOption t) where
    type DFields (EnumOption t) = (() ': '[])
    type DType (EnumOption t) = t
    type DContainer (EnumOption t) = PhantomEnumContainer
    compileRec x = PhantomEnumContainer [x]

instance (IsDataSpec x, IsDataSpec y, DType x ~ DType y, DContainer x ~ DContainer y, CombinableContainer (DContainer x)) => IsDataSpec (x :& y) where
    type DFields (x :& y) = HV.Append (DFields x) (DFields y)
    type DType (x :& y) = DType x
    type DContainer (x :& y) = DContainer x
    compileRec (x :& y) = combineContainer (compileRec x) (compileRec y)

recAppend :: RecordFields t as -> RecordFields t bs -> RecordFields t (HV.Append as bs)
recAppend RFEmpty bs = bs
recAppend (a :+: as) bs = a :+: (as `recAppend` bs)

sumAppend :: SumOptions t as -> SumOptions t bs -> SumOptions t (HV.Append as bs)
sumAppend SOEmpty bs = bs
sumAppend (a :|: as) bs = a :|: (as `sumAppend` bs)


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

type RecordTypeSpec t flds = HighSpec t 'SpecRecord flds

recSpec ::
    (IsDataSpec q, DContainer q ~ RecordFields)
    => T.Text -> Maybe T.Text -> HV.HVectElim (DFields q) (DType q)
    -> q
    -> RecordTypeSpec (DType q) (DFields q)
recSpec name mDesc mk fields =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecRecord $ RecordSpec (HV.uncurry mk) (compileRec fields)
    }

type SumTypeSpec t flds = HighSpec t 'SpecSum flds

sumSpec ::
    (IsDataSpec q, DContainer q ~ SumOptions)
    => T.Text -> Maybe T.Text -> q -> SumTypeSpec (DType q) (DFields q)
sumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecSum $ SumSpec (compileRec opts)
    }

type EnumTypeSpec t flds = HighSpec t 'SpecEnum flds

enumSpec ::
    (IsDataSpec q, DContainer q ~ PhantomEnumContainer)
    => T.Text -> Maybe T.Text -> q -> EnumTypeSpec (DType q) (DFields q)
enumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecEnum $ EnumSpec (unPhantomEnumContainer $ compileRec opts)
    }

enumOpt :: T.Text -> Prism' t () -> EnumOption t
enumOpt jsonKey p =
    EnumOption
    { eo_jsonKey = jsonKey
    , eo_prism = p
    }

(@->) :: T.Text -> Prism' t () -> EnumOption t
jsonKey @-> p = enumOpt jsonKey p
