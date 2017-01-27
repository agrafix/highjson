{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Data.HighJson.Serialiser
    ( -- * Description how to serialise JSON from a Haskell type
      ToJSON(..), Value(..)
      -- * DSL to easily create serialiser for custom Haskell types
    , runSerSpec, SerSpec(..), (.<-), KeyedSerialiser, SerObjSpec(..)
    , SpecKey(..), (.:), (.:?)
    )
where

import Data.Aeson (ToJSON(..), Value(..))
import Data.Typeable
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

-- | A json key and a getter
data SpecKey k t
   = SpecKey
   { k_key :: !T.Text
   , k_req :: !Bool
   , k_getVal :: !(k -> Maybe t)
   }

-- | Construct a 'SpecKey' mapping a json key to a getter function
(.:) :: T.Text -> (k -> t) -> SpecKey k t
k .: getter =
    SpecKey
    { k_key = k
    , k_req = True
    , k_getVal = Just . getter
    }
{-# INLINE (.:) #-}

-- | Construct a 'SpecKey' mapping a json key to a getter function of
-- a 'Maybe' type. This allows to omit the key when generating json instead of
-- setting it to null.
(.:?) :: T.Text -> (k -> Maybe t) -> SpecKey k (Maybe t)
k .:? getter =
    SpecKey k False $ \obj ->
        let val = getter obj
        in case val of
             Nothing -> Nothing
             Just _ -> Just val
{-# INLINE (.:?) #-}

data KeyedSerialiser k
    = KeyedSerialiser { _unKeyedSerialiser :: A.Pair }

-- | Associate a JSON key with a serialiser
(.<-) :: A.ToJSON a => T.Text -> a -> KeyedSerialiser k
a .<- b = KeyedSerialiser $ a A..= b
{-# INLINE (.<-) #-}

-- | Parser specification. Use 'OnlyConstr' for normal types and 'FirstConstr'/'NextConstr' for sum types
data SerSpec k where
    SingleConstr :: SerObjSpec k ts -> SerSpec k
    MultiConstr :: (k -> KeyedSerialiser k) -> SerSpec k

runSerSpec :: SerSpec k -> k -> A.Value
runSerSpec spec input =
    case spec of
      SingleConstr fullSpec -> runSerObjSpec fullSpec input
      MultiConstr getVal ->
          case getVal input of
            KeyedSerialiser ser -> A.object [ser]
{-# INLINE runSerSpec #-}

-- | List of 'SpecKey's defining the serialisation of values to json
data SerObjSpec k (ts :: [*]) where
    SerObjSpecNil :: SerObjSpec k '[]
    (:&&&:) :: (A.ToJSON t, Typeable t) => !(SpecKey k t) -> !(SerObjSpec k ts) -> SerObjSpec k (t ': ts)

infixr 5 :&&&:

-- | Convert a 'SerObjSpec' into an 'Value' for defining 'ToJson' instances
runSerObjSpec :: SerObjSpec k ts -> k -> A.Value
runSerObjSpec spec input = A.object (buildSpec spec input)
{-# INLINE runSerObjSpec #-}

buildSpec :: SerObjSpec k ts -> k -> [A.Pair]
buildSpec spec input =
    case spec of
      SerObjSpecNil -> mempty
      (SpecKey key _ getVal :&&&: xs) ->
          case getVal input of
            Nothing -> buildSpec xs input
            Just v -> (key A..= v) : buildSpec xs input
