{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Json.Parser
    ( -- * Parsing from different types
      parseJsonBs, parseJsonBsl, parseJsonT
      -- * Description how to parse JSON to a Haskell type
    , JsonReadable(..)
      -- * DSL to easily create parser for custom Haskell types
    , runSpec, ObjSpec(..)
    , TypedKey, reqKey, optKey
      -- * Low level JSON parsing helpers
    , readObject, WrappedValue(..), getValueByKey, getOptValueByKey
    )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.HVect
import Data.Maybe
import Data.Scientific hiding (scientific)
import Data.String
import Data.Typeable
import Prelude hiding (uncurry)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

-- | Parse json from a strict 'BS.ByteString'
parseJsonBs :: JsonReadable t => BS.ByteString -> Either String t
parseJsonBs = parseOnly (readJson <* skipSpace <* endOfInput)
{-# INLINE parseJsonBs #-}

-- | Parse json from a lazy 'BSL.ByteString'
parseJsonBsl :: JsonReadable t => BSL.ByteString -> Either String t
parseJsonBsl = parseJsonBs . BSL.toStrict
{-# INLINE parseJsonBsl #-}

-- | Parse json from a strict 'T.Text'
parseJsonT :: JsonReadable t => T.Text -> Either String t
parseJsonT = parseJsonBs . T.encodeUtf8
{-# INLINE parseJsonT #-}

-- | Typeclass defining an attoparsec 'Parser' how Haskell types should
-- be parsed from JSON. Use predifined instances (with 'readJson') and
-- 'runSpec' (on 'ObjSpec') to define instances for custom types
class JsonReadable t where
    readJson :: Parser t

instance JsonReadable t => JsonReadable [t] where
    readJson = readJList

instance JsonReadable t => JsonReadable (V.Vector t) where
    readJson = liftM V.fromList readJList

readJList :: JsonReadable t => Parser [t]
readJList =
    do skipSpace
       char '['
       vals <- readJson `sepBy` (skipSpace >> char ',')
       skipSpace
       char ']'
       return vals
{-# INLINE readJList #-}

instance JsonReadable Bool where
    readJson = readBool

readBool :: Parser Bool
readBool =
    do skipSpace
       True <$ string "true" <|> False <$ string "false"
{-# INLINE readBool #-}

instance JsonReadable Scientific where
    readJson = skipSpace >> scientific

instance JsonReadable Double where
    readJson = readDouble

readDouble :: Parser Double
readDouble = liftM toRealFloat readJson
{-# INLINE readDouble #-}

instance JsonReadable Int where
    readJson = readInt


readInt :: Parser Int
readInt =
    do mRes <- liftM toBoundedInteger readJson
       case mRes of
         Nothing -> fail "input is not an integer"
         Just val -> return val
{-# INLINE readInt #-}

instance JsonReadable T.Text where
    readJson = readText

readText :: Parser T.Text
readText =
    do skipSpace
       char '"'
       txt <-
           scan False $ \s c ->
           if s
           then Just False
           else if c == '"'
                then Nothing
                else Just (c == '\\')
       char '"'
       case T.decodeUtf8' txt of
         Right r -> return r
         Left msg -> fail $ show msg
{-# INLINE readText #-}

instance JsonReadable t => JsonReadable (Maybe t) where
    readJson = readMaybe

readMaybe :: JsonReadable t => Parser (Maybe t)
readMaybe =
    do skipSpace
       Nothing <$ string "null" <|> Just <$> readJson
{-# INLINE readMaybe #-}

instance (JsonReadable a, JsonReadable b) => JsonReadable (Either a b) where
    readJson = readEither

readEither :: (JsonReadable a, JsonReadable b) => Parser (Either a b)
readEither =
    Left <$> readJson <|> Right <$> readJson
{-# INLINE readEither #-}

-- | A value that is 'Typeable' and 'JsonReadable'
data WrappedValue
   = forall t. (Typeable t, JsonReadable t) => WrappedValue !t

-- | Parse a json object given a value parser for each key
readObject :: (T.Text -> Maybe (Parser WrappedValue)) -> Parser (HM.HashMap T.Text WrappedValue)
readObject getKeyParser =
    do skipSpace
       char '{'
       vals <- parseKv `sepBy` (skipSpace >> char ',')
       skipSpace
       char '}'
       return $! HM.fromList (catMaybes vals)
    where
      parseKv =
          do k <- readText
             skipSpace
             char ':'
             case getKeyParser k of
               Nothing -> return Nothing
               Just parser -> Just <$> ((,) <$> pure k <*> parser)
{-# INLINE readObject #-}

-- | Get a value out of the map returned by 'readObject'
getValueByKey :: (Monad m, Typeable t) => T.Text -> HM.HashMap T.Text WrappedValue -> m t
getValueByKey key hm =
    do optVal <- getOptValueByKey key hm
       case optVal of
         Nothing -> fail ("Key " ++ show key ++ " not present")
         Just val -> return val
{-# INLINE getValueByKey #-}

-- | Optionally get a value out of the map returned by 'readObject'
getOptValueByKey :: (Monad m, Typeable t) => T.Text -> HM.HashMap T.Text WrappedValue -> m (Maybe t)
getOptValueByKey key hm =
    case HM.lookup key hm of
      Just (WrappedValue x) ->
          case cast x of
            Just val -> return (Just val)
            Nothing -> fail "Invalid wrapped type"
      Nothing -> return Nothing
{-# INLINE getOptValueByKey #-}

type KeyReader t =
    Monad m => T.Text -> HM.HashMap T.Text WrappedValue -> m t

-- | Json object key to a value t
data TypedKey t =
    TypedKey !(KeyReader t) !T.Text

-- | Required json object key. Use 'IsString' instance for automatic choice
reqKey :: Typeable t => T.Text -> TypedKey t
reqKey = TypedKey getValueByKey

-- | Optional json object key. Use 'IsString' instance for automatic choice
optKey :: Typeable t => T.Text -> TypedKey (Maybe t)
optKey =
    TypedKey optGetter
    where
      optGetter k hm =
          do mOpt <- getOptValueByKey k hm
             return $ join mOpt

instance Typeable t => IsString (TypedKey (Maybe t)) where
    fromString = optKey . T.pack

instance Typeable t => IsString (TypedKey t) where
    fromString = reqKey . T.pack

-- | List of 'TypedKey's, should be in the same order as your
-- constructor in 'runSpec' will expect them
data ObjSpec (ts :: [*]) where
    ObjSpecNil :: ObjSpec '[]
    (:&&:) :: (JsonReadable t, Typeable t) => !(TypedKey t) -> !(ObjSpec ts) -> ObjSpec (t ': ts)

infixr 5 :&&:

type CompiledSpec m ts =
    (HM.HashMap T.Text WrappedValue -> m (HVect ts), T.Text -> Maybe (Parser WrappedValue))

compileSpec :: Monad m => ObjSpec ts -> CompiledSpec m ts
compileSpec ObjSpecNil = (const (return HNil), const Nothing)
compileSpec ((TypedKey keyReader key :: TypedKey t) :&&: xs) =
    let (nextHmFun, nextParserFun) = compileSpec xs
    in ( \hm ->
             do el <- keyReader key hm
                xs <- nextHmFun hm
                return (el :&: xs)
       , \lookupKey ->
           if lookupKey == key
           then Just (liftM WrappedValue (readJson :: Parser t))
           else nextParserFun lookupKey
       )

-- | Convert an 'ObjSpec' into a 'Parser' provided a constructor
-- function
runSpec :: HVectElim ts x -> ObjSpec ts -> Parser x
runSpec mkVal spec =
    do let (mkTyVect, kv) = compileSpec spec
       hm <- readObject kv
       vect <- mkTyVect hm
       return $! uncurry mkVal vect
