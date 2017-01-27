{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.HighJson.Parser
    ( FromJSON(..)
      -- * DSL to easily create parser for custom Haskell types
    , runParseSpec, ObjSpec(..), ParseSpec(..), KeyedConstr, (.->), (<||>)
    , ConstrTagger, ResultType
    , TypedKey(..), reqKey, optKey, typedKeyKey
      -- * Low level JSON parsing helpers
    , Parser
    )
where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.HVect
import Data.Maybe
import Data.Typeable
import Prelude hiding (uncurry, take)
import qualified Data.Text as T

type KeyReader t =
    T.Text -> Object -> Parser t

-- | Json object key to a value t
data TypedKey t =
    TypedKey !(KeyReader t) !T.Text

-- | Get the textual key of a 'TypedKey'
typedKeyKey :: TypedKey t -> T.Text
typedKeyKey (TypedKey _ t) = t
{-# INLINE typedKeyKey #-}

-- | Required json object key. Use 'IsString' instance for automatic choice
reqKey :: (FromJSON t) => T.Text -> TypedKey t
reqKey = TypedKey (flip (.:))
{-# INLINE reqKey #-}

-- | Optional json object key. Use 'IsString' instance for automatic choice
optKey :: (FromJSON t) => T.Text -> TypedKey (Maybe t)
optKey =
    TypedKey optGetter
    where
      optGetter k hm =
          do mOpt <- hm .:? k
             return $ join mOpt
{-# INLINE optKey #-}

-- | Associates a json key with a parser
data KeyedConstr k
   = KeyedConstr
   { kc_key :: !T.Text
   , kc_parser :: !(Value -> Parser k)
   }

class ConstrTagger r where
    type ResultType r :: *
    -- | Associate a json key with a parser
    (.->) :: forall a. FromJSON a => T.Text -> (a -> ResultType r) -> r

instance ConstrTagger (KeyedConstr k) where
    type ResultType (KeyedConstr k) = k
    key .-> mk = KeyedConstr { kc_key = key, kc_parser = \v -> mk <$> parseJSON v }

instance ConstrTagger (ParseSpec k) where
    type ResultType (ParseSpec k) = k
    key .-> mk = FirstConstr (KeyedConstr key $ \v -> mk <$> parseJSON v)

-- | Parser specification. Use ':$:' for normal types and 'FirstConstr' / ':|:' for sum types
data ParseSpec k where
    (:$:) :: HVectElim ts k -> ObjSpec ts -> ParseSpec k
    FirstConstr :: KeyedConstr k -> ParseSpec k
    (:|:) :: KeyedConstr k -> ParseSpec k -> ParseSpec k

infixr 4 :$:
infixr 3 <||>

-- | Choice between multiple constructors
(<||>) :: KeyedConstr k -> ParseSpec k -> ParseSpec k
(<||>) = (:|:)
{-# INLINE (<||>) #-}

-- | Convert a 'ParseSpec' into a 'Parser'
runParseSpec :: ParseSpec k -> Value -> Parser k
runParseSpec x v =
    case x of
      constr :$: spec ->
          flip (withObject "toplevel") v $ \obj ->
          runSpec constr spec obj
      FirstConstr (KeyedConstr key parser) ->
          flip (withObject (T.unpack key)) v $ \obj ->
          do val <- obj .: key
             parser val
      constr :|: next ->
          runParseSpec (FirstConstr constr) v <|> runParseSpec next v

-- | List of 'TypedKey's, should be in the same order as your
-- constructor in 'runSpec' will expect them
data ObjSpec (ts :: [*]) where
    ObjSpecNil :: ObjSpec '[]
    (:&&:) :: (FromJSON t, Typeable t) => !(TypedKey t) -> !(ObjSpec ts) -> ObjSpec (t ': ts)

infixr 5 :&&:

compileSpec :: ObjSpec ts -> Object -> Parser (HVect ts)
compileSpec ObjSpecNil _ = pure HNil
compileSpec ((TypedKey keyReader key :: TypedKey t) :&&: xs) obj =
    do el <- keyReader key obj
       more <- compileSpec xs obj
       pure (el :&: more)

-- | Convert an 'ObjSpec' into a 'Parser' provided a constructor
-- function for defining 'JsonReadable' instances.
runSpec :: HVectElim ts x -> ObjSpec ts -> Object -> Parser x
runSpec mkVal spec obj = uncurry mkVal <$> compileSpec spec obj
