{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Json.Parser
    ( -- * Parsing from different types
      parseJsonBs, parseJsonBsl, parseJsonT
      -- * Description how to parse JSON to a Haskell type
    , JsonReadable(..)
      -- * DSL to easily create parser for custom Haskell types
    , runParseSpec, ObjSpec(..), ParseSpec(..), KeyedConstr, (.->), (<||>)
    , ConstrTagger, ResultType
    , TypedKey, reqKey, optKey, typedKeyKey
      -- * Low level JSON parsing helpers
    , readObject, Parser, WrappedValue(..), getValueByKey, getOptValueByKey
    )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString as ABS
import Data.HVect
import Data.Int
import Data.Maybe
import Data.Scientific hiding (scientific)
import Data.String
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (uncurry, take)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

foreign import ccall unsafe "bs_json_unescape" bs_json_unescape :: CULong -> Ptr CInt -> CString -> CString -> IO ()

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
    readJson = readJList readJson

instance JsonReadable t => JsonReadable (V.Vector t) where
    readJson = liftM V.fromList readJson

readJList :: Parser t -> Parser [t]
readJList parseEl =
    do skipSpace
       char '['
       vals <- parseEl `sepBy'` (skipSpace >> char ',')
       skipSpace
       char ']'
       return vals
{-# INLINE readJList #-}

readTuple :: JsonReadable t => Parser (t, t)
readTuple =
    do xs <- readJson
       case xs of
         (a : b : _) -> return (a, b)
         _ -> fail "Not a tuple!"
{-# INLINE readTuple #-}

instance JsonReadable t => JsonReadable (t, t) where
    readJson = readTuple

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
    readJson = readBoundedInteger

instance JsonReadable Int8 where
    readJson = readBoundedInteger

instance JsonReadable Int16 where
    readJson = readBoundedInteger

instance JsonReadable Int32 where
    readJson = readBoundedInteger

instance JsonReadable Int64 where
    readJson = readBoundedInteger

instance JsonReadable Word where
    readJson = readBoundedInteger

instance JsonReadable Word8 where
    readJson = readBoundedInteger

instance JsonReadable Word16 where
    readJson = readBoundedInteger

instance JsonReadable Word32 where
    readJson = readBoundedInteger

instance JsonReadable Word64 where
    readJson = readBoundedInteger

readBoundedInteger :: (Integral i, Bounded i) => Parser i
readBoundedInteger =
     do mRes <- liftM toBoundedInteger readJson
        case mRes of
          Nothing -> fail "input is not a bounded integer"
          Just val -> return val
{-# INLINE readBoundedInteger #-}

instance JsonReadable T.Text where
    readJson = readText

data StrictScan
   = StrictScan {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

readText :: Parser T.Text
readText =
    do skipSpace
       char '"'
       (txt, (StrictScan _ escaped)) <-
           ABS.runScanner (StrictScan 0 0) strictScan
       char '"'
       readyForRead <-
               if escaped == 1
               then case unescapeText txt of
                      Left err ->
                          fail err
                      Right ok ->
                          return ok
               else return txt
       case T.decodeUtf8' readyForRead of
         Right r -> return r
         Left msg -> fail $ show msg
    where
      strictScan !(StrictScan x y) !c =
          if x == 1 then Just (StrictScan 0 y)
          else if c == 34 then Nothing -- '"'
               else let x' = if c == 92 then 1 else 0 -- '\\'
                    in Just (StrictScan x' (max x' y))
{-# INLINE readText #-}

unescapeText :: BS.ByteString -> Either String BS.ByteString
unescapeText bs =
    unsafePerformIO $
    do let len = BS.length bs
       outBsPtr <- BS.mallocByteString len
       (outBs, errCode) <-
           withForeignPtr outBsPtr $ \ptr ->
           alloca $ \errCode ->
           BS.unsafeUseAsCString bs $ \inBs ->
           do bs_json_unescape (fromIntegral len) errCode inBs ptr
              code <- peek errCode
              bs <- BS.unsafePackCString ptr
              return (bs, code)
       return $ if errCode /= 0
                then Left ("Invalid escape sequence. ErrNo=" ++ show errCode)
                else Right outBs

instance JsonReadable t => JsonReadable (Maybe t) where
    readJson = readMaybe

readNull :: Parser ()
readNull = () <$ string "null"
{-# INLINE readNull #-}

readMaybe :: JsonReadable t => Parser (Maybe t)
readMaybe =
    do skipSpace
       Nothing <$ readNull <|> Just <$> readJson
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

readAnyJsonVal :: Parser ()
readAnyJsonVal =
    () <$ readObject (const Nothing)
           <|> () <$ readBool
           <|> () <$ readText
           <|> () <$ readNull
           <|> () <$ (skipSpace >> scientific)
           <|> () <$ readJList readAnyJsonVal
{-# INLINE readAnyJsonVal #-}

instance JsonReadable a => JsonReadable (HVect '[a]) where
    readJson = liftM singleton readJson

-- | Parse a json object given a value parser for each key
readObject :: (T.Text -> Maybe (Parser a)) -> Parser (HM.HashMap T.Text a)
readObject getKeyParser =
    do skipSpace
       char '{'
       vals <- kvLoop
       skipSpace
       char '}'
       skipSpace
       return $! vals
    where
      kvLoop =
          do skipSpace
             val <- parseKv
             skipSpace
             ch <- peekChar'
             hm <-
                 if ch == ','
                 then do char ','
                         kvLoop
                 else return HM.empty
             return $
                    case val of
                      Just (k, v) -> HM.insert k v hm
                      Nothing -> hm
      parseKv =
          do k <- readText
             skipSpace
             char ':'
             case getKeyParser k of
               Nothing ->
                   do readAnyJsonVal
                      return Nothing
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

-- | Get the textual key of a 'TypedKey'
typedKeyKey :: TypedKey t -> T.Text
typedKeyKey (TypedKey _ t) = t
{-# INLINE typedKeyKey #-}

-- | Required json object key. Use 'IsString' instance for automatic choice
reqKey :: Typeable t => T.Text -> TypedKey t
reqKey = TypedKey getValueByKey
{-# INLINE reqKey #-}

-- | Optional json object key. Use 'IsString' instance for automatic choice
optKey :: Typeable t => T.Text -> TypedKey (Maybe t)
optKey =
    TypedKey optGetter
    where
      optGetter k hm =
          do mOpt <- getOptValueByKey k hm
             return $ join mOpt
{-# INLINE optKey #-}

instance Typeable t => IsString (TypedKey (Maybe t)) where
    fromString = optKey . T.pack

instance Typeable t => IsString (TypedKey t) where
    fromString = reqKey . T.pack

-- | Associates a json key with a parser
data KeyedConstr k
   = KeyedConstr
   { kc_key :: !T.Text
   , kc_parser :: !(Parser k)
   }

class ConstrTagger r where
    type ResultType r :: *
    -- | Associate a json key with a parser
    (.->) :: T.Text -> Parser (ResultType r) -> r

instance ConstrTagger (KeyedConstr k) where
    type ResultType (KeyedConstr k) = k
    key .-> parser = KeyedConstr key parser

instance ConstrTagger (ParseSpec k) where
    type ResultType (ParseSpec k) = k
    key .-> parser = FirstConstr (KeyedConstr key parser)

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
runParseSpec :: ParseSpec k -> Parser k
runParseSpec x =
    case x of
      constr :$: spec ->
          runSpec constr spec
      FirstConstr (KeyedConstr key parser) ->
          let keyGetter reqKey =
                  if reqKey == key
                  then Just parser
                  else Nothing
          in do hm <- readObject keyGetter
                case HM.lookup key hm of
                  Nothing -> fail ("Missing key " ++ show key)
                  Just x -> return x
      constr :|: next ->
          runParseSpec (FirstConstr constr) <|> runParseSpec next

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
           then Just $! liftM WrappedValue (readJson :: Parser t)
           else nextParserFun lookupKey
       )

-- | Convert an 'ObjSpec' into a 'Parser' provided a constructor
-- function for defining 'JsonReadable' instances.
runSpec :: HVectElim ts x -> ObjSpec ts -> Parser x
runSpec mkVal spec =
    do let (mkTyVect, kv) = compileSpec spec
       !hm <- readObject kv
       !vect <- mkTyVect hm
       return $! uncurry mkVal vect
