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
    , IsRecordSpec(..), (:&)(..)
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

class IsRecordSpec t where
    type RFields t :: [*]
    type RType t
    type RContainer t :: * -> [*] -> *
    compileRec :: t -> (RContainer t) (RType t) (RFields t)

instance IsRecordSpec (RecordField t f) where
    type RFields (RecordField t f) = (f ': '[])
    type RType (RecordField t f) = t
    type RContainer (RecordField t f) = RecordFields
    compileRec x = x :+: RFEmpty

instance IsRecordSpec (SumOption t f) where
    type RFields (SumOption t f) = (f ': '[])
    type RType (SumOption t f) = t
    type RContainer (SumOption t f) = SumOptions
    compileRec x = x :|: SOEmpty

newtype PhantomEnumContainer t (ts :: [*])
    = PhantomEnumContainer { unPhantomEnumContainer :: [EnumOption t] }

instance IsRecordSpec (EnumOption t) where
    type RFields (EnumOption t) = (() ': '[])
    type RType (EnumOption t) = t
    type RContainer (EnumOption t) = PhantomEnumContainer
    compileRec x = PhantomEnumContainer [x]

instance (IsRecordSpec x, IsRecordSpec y, RType x ~ RType y, RContainer x ~ RContainer y, CombinableContainer (RContainer x)) => IsRecordSpec (x :& y) where
    type RFields (x :& y) = HV.Append (RFields x) (RFields y)
    type RType (x :& y) = RType x
    type RContainer (x :& y) = RContainer x
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
    (IsRecordSpec q, RContainer q ~ RecordFields)
    => T.Text -> Maybe T.Text -> HV.HVectElim (RFields q) (RType q)
    -> q
    -> RecordTypeSpec (RType q) (RFields q)
recSpec name mDesc mk fields =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecRecord $ RecordSpec (HV.uncurry mk) (compileRec fields)
    }

type SumTypeSpec t flds = HighSpec t 'SpecSum flds

sumSpec ::
    (IsRecordSpec q, RContainer q ~ SumOptions)
    => T.Text -> Maybe T.Text -> q -> SumTypeSpec (RType q) (RFields q)
sumSpec name mDesc opts =
    HighSpec
    { hs_name = name
    , hs_description = mDesc
    , hs_bodySpec = BodySpecSum $ SumSpec (compileRec opts)
    }

type EnumTypeSpec t flds = HighSpec t 'SpecEnum flds

enumSpec ::
    (IsRecordSpec q, RContainer q ~ PhantomEnumContainer)
    => T.Text -> Maybe T.Text -> q -> EnumTypeSpec (RType q) (RFields q)
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
