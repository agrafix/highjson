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
    , recSpec, RecordTypeSpec, reqField, (.=), optField, (.=?)
      -- * Construct specifications for sum types
    , sumSpec, SumTypeSpec, sumOpt, (.->)
      -- * Construct specifications for enum types
    , enumSpec, EnumTypeSpec, enumOpt, (@->)
      -- * Shared between specifications for simplicity
    , IsDataSpec(..), (:&)(..)
      -- * Generate json serializers/encoders and parsers from specs
    , jsonSerializer, jsonEncoder, jsonParser
      -- * Specification structures
    , BodySpec(..)
    , RecordField(..), RecordSpec(..), RecordFields(..)
    , SumOption(..), SumSpec(..), SumOptions(..)
    , EnumOption(..), EnumSpec(..)
      -- * Aeson reexports
    , ToJSON(..), FromJSON(..)
      -- * Implementation detail structures
    , PhantomEnumContainer(..), CombinableContainer(..)
    )
where

import Data.HighJson.Types

import Control.Lens hiding ((.=))
import Data.Aeson ((.:), (.:?), FromJSON(..), ToJSON(..))
import Data.Typeable
import qualified Data.HVect as HV
import qualified Data.Text as T

-- | Combination of two local specifications. For records, these are fields, for sum types and enums
-- these are the options.
data a :& b
    = a :& b
    deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 8 :&

instance (Semigroup a, Semigroup b) => Semigroup (a :& b) where
    (a :& b) <> (a' :& b') = (a <> a') :& (b <> b')

instance (Monoid a, Monoid b) => Monoid (a :& b) where
    mempty = mempty :& mempty

-- | A monoidal type class that respects type level lists associated to the bodies
class CombinableContainer t where
    combineContainer :: t a (as :: [*]) -> t a (bs :: [*]) -> t a (HV.Append as bs)

instance CombinableContainer RecordFields where
    combineContainer = recAppend

instance CombinableContainer SumOptions where
    combineContainer = sumAppend

instance CombinableContainer PhantomEnumContainer where
    combineContainer (PhantomEnumContainer x) (PhantomEnumContainer y) =
        PhantomEnumContainer $ x ++ y

-- | A type class that allows a unified notation for records and sum types. Build specifications
-- using '(:&)' and '(.=)', '(.=?)', '(.->)' or '(@->)'
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

-- | A required json field. The key must be present in the json.
reqField :: FromJSON f => T.Text -> (t -> f) -> RecordField t f
reqField jsonKey g =
    RecordField
    { rf_jsonKey = jsonKey
    , rf_optional = False
    , rf_jsonLoader = (.:)
    , rf_get = g
    }

-- | Alias for 'reqField'
(.=) :: FromJSON f =>  T.Text -> (t -> f) -> RecordField t f
jsonKey .= reader = reqField jsonKey reader

-- | An optional json field.
optField :: FromJSON f => T.Text -> (t -> Maybe f) -> RecordField t (Maybe f)
optField jsonKey g =
    RecordField
    { rf_jsonKey = jsonKey
    , rf_optional = True
    , rf_jsonLoader = (.:?)
    , rf_get = g
    }

-- | Alias for 'optField'
(.=?) :: FromJSON f =>  T.Text -> (t -> Maybe f) -> RecordField t (Maybe f)
name .=? reader = optField name reader

-- | An option of a sum type
sumOpt :: T.Text -> Prism' t o -> SumOption t o
sumOpt jsonKey p =
    SumOption
    { so_jsonKey = jsonKey
    , so_prism = p
    }

-- | Alias for 'sumOpt'
(.->) :: T.Text -> Prism' t o -> SumOption t o
jsonKey .-> p = sumOpt jsonKey p

-- | An option of a classic enum
enumOpt :: T.Text -> Prism' t () -> EnumOption t
enumOpt jsonKey p =
    EnumOption
    { eo_jsonKey = jsonKey
    , eo_prism = p
    }

-- | Alias for 'enumOpt'
(@->) :: T.Text -> Prism' t () -> EnumOption t
jsonKey @-> p = enumOpt jsonKey p

-- | A specification for a record
type RecordTypeSpec t flds = HighSpec t 'SpecRecord flds

-- | The specification for a record. Contains a name, an optional description,
-- the constructor and a description how to parse and serialize fields respecting
-- a given json key.
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

-- | A specification for an arbitrary sum type
type SumTypeSpec t flds = HighSpec t 'SpecSum flds

-- | The specification for a sum type. Contains a name, an optional description
-- and a mapping from all constructor (prims) to their respective json fields
sumSpec ::
    (IsDataSpec q, DContainer q ~ SumOptions)
    => T.Text -> Maybe T.Text -> q -> SumTypeSpec (DType q) (DFields q)
sumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecSum $ SumSpec (compileRec opts)
    }

-- | A specification for a classic enum
type EnumTypeSpec t flds = HighSpec t 'SpecEnum flds

-- | The specification for a classic enum type. Contains a name, an optional description
-- and a mapping from all constructors to ther counterpart json string names.
enumSpec ::
    (IsDataSpec q, DContainer q ~ PhantomEnumContainer)
    => T.Text -> Maybe T.Text -> q -> EnumTypeSpec (DType q) (DFields q)
enumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecEnum $ EnumSpec (unPhantomEnumContainer $ compileRec opts)
    }
