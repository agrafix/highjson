{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Data.Json.Serialiser
    ( -- * Serialising to different types
      serialiseJsonBs, serialiseJsonBsl, serialiseJsonT
      -- * Description how to serialise JSON from a Haskell type
    , ToJson(..)
      -- * DSL to easily create serialiser for custom Haskell types
    , runSerSpec, SerSpec(..)
    , SpecKey, (.:), (.:?)
      -- * Low level JSON serialising helpers
    , ObjectBuilder, emptyObject, Value, (.=), (.=#), row, array, nullValue
    )
where

import Data.BufferBuilder.Json
import Data.Monoid
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

instance (ToJson a, ToJson b) => ToJson (Either a b) where
    toJson x =
        case x of
          Left y -> toJson y
          Right z -> toJson z

-- | A json key and a getter
data SpecKey k t
   = SpecKey
   { k_key :: !T.Text
   , k_getVal :: !(k -> Maybe t)
   }

(.:) :: (ToJson t, Typeable t) => T.Text -> (k -> t) -> SpecKey k t
k .: getter = SpecKey k (Just . getter)
{-# INLINE (.:) #-}

(.:?) :: (ToJson t, Typeable t) => T.Text -> (k -> Maybe t) -> SpecKey k (Maybe t)
k .:? getter =
    SpecKey k $ \obj ->
        let val = getter obj
        in case val of
             Nothing -> Nothing
             Just _ -> Just val
{-# INLINE (.:?) #-}

-- | List of 'SpecKey's defining the serialisation of values to json
data SerSpec k (ts :: [*]) where
    SerSpecNil :: SerSpec k '[]
    (:&&&:) :: (ToJson t, Typeable t) => !(SpecKey k t) -> !(SerSpec k ts) -> SerSpec k (t ': ts)

infixr 5 :&&&:

-- | Convert a 'SerSpec' into an 'Value' for defining 'ToJson' instances
runSerSpec :: SerSpec k ts -> k -> Value
runSerSpec spec input =
    toJson (buildSpec spec input)

buildSpec :: SerSpec k ts -> k -> ObjectBuilder
buildSpec spec input =
    case spec of
      SerSpecNil -> mempty
      (SpecKey key getVal :&&&: xs) ->
          case getVal input of
            Nothing -> buildSpec xs input
            Just val -> key .= getVal input <> buildSpec xs input

-- | Serialise json to a strict 'BS.ByteString'
serialiseJsonBs :: ToJson a => a -> BS.ByteString
serialiseJsonBs = encodeJson
{-# INLINE serialiseJsonBs #-}

-- | Serialise json to a lazy 'BSL.ByteString'
serialiseJsonBsl :: ToJson a => a -> BSL.ByteString
serialiseJsonBsl = BSL.fromStrict . serialiseJsonBs
{-# INLINE serialiseJsonBsl #-}

-- | Serialise json to a strict 'T.Text'
serialiseJsonT :: ToJson a => a -> T.Text
serialiseJsonT = T.decodeUtf8 . serialiseJsonBs
{-# INLINE serialiseJsonT #-}