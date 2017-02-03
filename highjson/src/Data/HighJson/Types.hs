{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HighJson2 where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.HVect
import Data.Monoid
import qualified Data.Text as T

data HighSpec a as
    = HighSpec
    { hs_name :: !T.Text
    , hs_description :: !(Maybe T.Text)
    , hs_bodySpec :: !(BodySpec a as)
    }

data BodySpec a as
    = BodySpecRecord !(RecordSpec a as)
    | BodySpecSum !(SumSpec a as)

data RecordFields t fs where
    RFEmpty :: RecordFields t '[]
    RFField :: RecordField t f -> RecordFields t fs -> RecordFields t (f ': fs)

data RecordField t f
    = RecordField
    { rf_name :: !T.Text
    , rf_jsonKey :: !T.Text
    , rf_optional :: !Bool
    , rf_jsonLoader :: Object -> T.Text -> Parser f
    , rf_lens :: !(Lens' t f)
    }

data RecordSpec a fs
    = RecordSpec
    { rs_make :: HVect fs -> a
    , rs_fields :: RecordFields a fs
    }

data SumOptions t os where
    SOEmpty :: SumOptions t '[]
    SOOpt :: SumOption t o -> SumOptions t os -> SumOptions t (o ': os)

data SumOption t o
    = SumOption
    { so_name:: !T.Text
    , so_jsonKey :: !T.Text
    , so_prism :: !(Prism' t o)
    }

data SumSpec a os
    = SumSpec
    { ss_options :: SumOptions a os
    }

jsonSerializer :: AllHave ToJSON as => HighSpec a as -> a -> Value
jsonSerializer hs val =
    object $ fst $
    case hs_bodySpec hs of
      BodySpecSum s -> jsonSerSum s val
      BodySpecRecord r -> jsonSerRec r val

jsonEncoder :: AllHave ToJSON as => HighSpec a as -> a -> Encoding
jsonEncoder hs val =
    pairs $ snd $
    case hs_bodySpec hs of
      BodySpecSum s -> jsonSerSum s val
      BodySpecRecord r -> jsonSerRec r val

jsonSerSum :: forall a as. AllHave ToJSON as => SumSpec a as -> a -> ([Pair], Series)
jsonSerSum (SumSpec sopts) val =
    loop sopts
    where
      loop ::
          forall fs. AllHave ToJSON fs => SumOptions a fs -> ([Pair], Series)
      loop flds =
          case flds of
            SOEmpty -> ([], mempty)
            SOOpt f fs ->
                case val ^? so_prism f of
                  Just body ->
                      let pair = (so_jsonKey f, toJSON body)
                          encoder = so_jsonKey f .= body
                      in ([pair], encoder)
                  Nothing -> loop fs

jsonSerRec :: forall a as. AllHave ToJSON as => RecordSpec a as -> a -> ([Pair], Series)
jsonSerRec (RecordSpec _ rflds) val =
    loop rflds ([], mempty)
    where
      loop ::
          forall fs. AllHave ToJSON fs
          => RecordFields a fs -> ([Pair], Series) -> ([Pair], Series)
      loop flds accum@(pairs, encoding) =
          case flds of
            RFEmpty -> accum
            RFField f fs ->
                let pair = (rf_jsonKey f, toJSON $ val ^. (rf_lens f))
                    encoder = rf_jsonKey f .= (val ^. (rf_lens f))
                in loop fs ((pair : pairs), encoder <> encoding)

jsonParser :: AllHave FromJSON as => HighSpec a as -> Value -> Parser a
jsonParser hs =
    withObject (T.unpack $ hs_name hs) $ \obj ->
    case hs_bodySpec hs of
      BodySpecRecord r -> jsonParserRecord r obj
      BodySpecSum s -> jsonParserSum (hs_name hs) s obj

jsonParserRecord :: forall a as. AllHave FromJSON as => RecordSpec a as -> Object -> Parser a
jsonParserRecord (RecordSpec mk rflds) obj =
    mk <$> loop rflds
    where
      loop :: forall fs. AllHave FromJSON fs => RecordFields a fs -> Parser (HVect fs)
      loop flds =
          case flds of
            RFEmpty -> pure HNil
            RFField f fs ->
                let parse =
                        rf_jsonLoader f obj (rf_jsonKey f)
                in do x <- parse
                      xs <- loop fs
                      pure (x :&: xs)

jsonParserSum :: forall a as. AllHave FromJSON as => T.Text -> SumSpec a as -> Object -> Parser a
jsonParserSum name (SumSpec sopts) obj =
    loop sopts
    where
      loop :: forall os. AllHave FromJSON os => SumOptions a os -> Parser a
      loop opts =
          case opts of
            SOEmpty ->
                fail $
                "Failed to parse as " ++ T.unpack name
            SOOpt o os ->
                let parse =
                        liftM (so_prism o #) $ obj .: so_jsonKey o
                in parse <|> loop os
