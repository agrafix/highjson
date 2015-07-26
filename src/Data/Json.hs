{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Data.Json
    ( -- * DSL to define JSON structure
      JsonSpec(..), FieldSpec(..), FieldKey, P.reqKey, P.optKey, P.TypedKey
    , (.=), (.=?)
      -- * DSL to define JSON structure for sum types
    , JsonSumSpec(..), (P..->), (P.<||>), (S..<-)
    , -- * Make parsers and serialisers from spec
      makeParser, makeSerialiser, makeSumParser, makeSumSerialiser
    , S.ToJson(..), P.JsonReadable(..)
    , -- * Run parsers / serialisers
      P.parseJsonBs, P.parseJsonBsl, P.parseJsonT
    , S.serialiseJsonBs, S.serialiseJsonBsl, S.serialiseJsonT
    )
where

import Data.HVect
import Data.Typeable
import qualified Data.Json.Serialiser as S
import qualified Data.Json.Parser as P

-- | Describes JSON parsing and serialisation of a Haskell type
data JsonSpec k (ts :: [*])
   = JsonSpec
   { j_constr :: !(HVectElim ts k)
   , j_fields :: !(FieldSpec k ts)
   }

-- | Describes JSON parsing and serialisation of a list of fields
data FieldSpec k (ts :: [*]) where
    EmptySpec :: FieldSpec k '[]
    (:+:) :: (S.ToJson t, P.JsonReadable t, Typeable t) => !(FieldKey k t) -> !(FieldSpec k ts) -> FieldSpec k (t ': ts)

infixr 5 :+:

-- | Describes a json key
data FieldKey k t
   = FieldKey
   { fk_tk :: !(P.TypedKey t)
   , fk_sk :: !(S.SpecKey k t)
   }

-- | Construct a 'FieldKey' mapping a json key to a getter function
(.=) :: (S.ToJson t, P.JsonReadable t, Typeable t) => P.TypedKey t -> (k -> t) -> FieldKey k t
tk .= getter = FieldKey tk ((P.typedKeyKey tk) S..: getter)
{-# INLINE (.=) #-}

-- | Construct a 'FieldKey' mapping a json key to a getter function of
-- a 'Maybe' type. This allows to omit the key when generating json instead of
-- setting it to null.
(.=?) :: (S.ToJson t, P.JsonReadable t, Typeable t) => P.TypedKey (Maybe t) -> (k -> Maybe t) -> FieldKey k (Maybe t)
tk .=? getter = FieldKey tk ((P.typedKeyKey tk) S..:? getter)
{-# INLINE (.=?) #-}

-- | Construct a 'P.Parser' from 'JsonSpec' to implement 'P.JsonReadable' instances
makeParser :: JsonSpec k ts -> P.Parser k
makeParser spec = P.runParseSpec $ (j_constr spec) P.:$: (mkObjSpec $ j_fields spec)
{-# INLINE makeParser #-}

mkObjSpec :: FieldSpec k ts -> P.ObjSpec ts
mkObjSpec EmptySpec = P.ObjSpecNil
mkObjSpec (FieldKey k _ :+: xs) = k P.:&&: mkObjSpec xs

-- | Construct a function from 'JsonSpec' to implement 'S.ToJson' instances
makeSerialiser :: JsonSpec k ts -> k -> S.Value
makeSerialiser spec = S.runSerSpec (S.SingleConstr $ mkSerSpec (j_fields spec))
{-# INLINE makeSerialiser #-}

mkSerSpec :: FieldSpec k ts -> S.SerObjSpec k ts
mkSerSpec EmptySpec = S.SerObjSpecNil
mkSerSpec (FieldKey _ getter :+: xs) = getter S.:&&&: mkSerSpec xs

-- | Describes JSON parsing and serialisation of a Haskell sum type. Currently
-- the library can only guarantee matching parsers/serialisers for
-- non-sum types using 'JsonSpec'.
data JsonSumSpec k
   = JsonSumSpec
   { js_parser :: !P.ParseSpec k
   , js_serialiser :: !(k -> S.KeyedSerialiser k)
   }

-- | Construct a 'P.Parser' from 'JsonSumSpec' to implement 'P.JsonReadable' instances
makeSumParser :: JsonSumSpec k -> P.Parser k
makeSumParser = P.runParseSpec . js_parser
{-# INLINE makeSumParser #-}

-- | Construct a function from 'JsonSumSpec' to implement 'S.ToJson' instances
makeSumSerialiser :: JsonSumSpec k -> k -> S.Value
makeSumSerialiser s = S.runSerSpec (S.MultiConstr (js_serialiser s))
{-# INLINE makeSumSerialiser #-}
