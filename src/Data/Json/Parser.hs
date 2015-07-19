{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Json.Parser where

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
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

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

data WrappedValue
   = forall t. (Typeable t, JsonReadable t) => WrappedValue t

readObject :: (T.Text -> Maybe (Parser WrappedValue)) -> Parser (HM.HashMap T.Text WrappedValue)
readObject getKeyParser =
    do skipSpace
       char '{'
       vals <- parseKv `sepBy` (skipSpace >> char ',')
       skipSpace
       char '}'
       return $ HM.fromList (catMaybes vals)
    where
      parseKv =
          do k <- readText
             skipSpace
             char ':'
             case getKeyParser k of
               Nothing -> return Nothing
               Just parser -> Just <$> ((,) <$> pure k <*> parser)
{-# INLINE readObject #-}

getValueByKey :: (Monad m, Typeable t) => T.Text -> HM.HashMap T.Text WrappedValue -> m t
getValueByKey key hm =
    do optVal <- getOptValueByKey key hm
       case optVal of
         Nothing -> fail ("Key " ++ show key ++ " not present")
         Just val -> return val
{-# INLINE getValueByKey #-}

getOptValueByKey :: (Monad m, Typeable t) => T.Text -> HM.HashMap T.Text WrappedValue -> m (Maybe t)
getOptValueByKey key hm =
    case HM.lookup key hm of
      Just (WrappedValue x) ->
          case cast x of
            Just val -> return (Just val)
            Nothing -> fail "Invalid wrapped type"
      Nothing -> return Nothing
{-# INLINE getOptValueByKey #-}

data TypedText t =
    TypedText T.Text

instance IsString (TypedText t) where
    fromString = TypedText . T.pack

data ObjSpec (ts :: [*]) where
    ObjSpecNil :: ObjSpec '[]
    (:&&:) :: (JsonReadable t, Typeable t) => TypedText t -> ObjSpec ts -> ObjSpec (t ': ts)

infixr 5 :&&:

type CompiledSpec m ts =
    (HM.HashMap T.Text WrappedValue -> m (HVect ts), T.Text -> Maybe (Parser WrappedValue))

compileSpec :: Monad m => ObjSpec ts -> CompiledSpec m ts
compileSpec ObjSpecNil = (const (return HNil), const Nothing)
compileSpec ((TypedText key :: TypedText t) :&&: xs) =
    let (nextHmFun, nextParserFun) = compileSpec xs
    in ( \hm ->
             do el <- getValueByKey key hm
                xs <- nextHmFun hm
                return (el :&: xs)
       , \lookupKey ->
           if lookupKey == key
           then Just (liftM WrappedValue (readJson :: Parser t))
           else nextParserFun lookupKey
       )

runSpec :: HVectElim ts x -> ObjSpec ts -> Parser x
runSpec mkVal spec =
    do let (mkTyVect, kv) = compileSpec spec
       hm <- readObject kv
       vect <- mkTyVect hm
       return $ uncurry mkVal vect

-- example:

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_either :: Either Bool T.Text
   } deriving (Show)

instance JsonReadable SomeDummy where
    readJson =
        runSpec SomeDummy $ "int" :&&: "bool" :&&: "text" :&&: "either" :&&: ObjSpecNil

testSomeDummy :: Result SomeDummy
testSomeDummy =
    parse readJson "{\"int\": 34, \"bool\": true, \"text\": \"Teext\", \"either\": false}"
