{-# LANGUAGE TemplateHaskell #-}
module Data.HighJson.TH
    ( deriveJsonSwagger
    , jsonSerializer, jsonEncoder, jsonParser
    , makeDeclareNamedSchema
    , ToSchema(..)
    , ToJSON(..), FromJSON(..)
    )
where

import Data.Aeson
import Data.Swagger
import Language.Haskell.TH

import Data.HighJson (jsonSerializer, jsonEncoder, jsonParser)
import Data.HighJson.Swagger (makeDeclareNamedSchema)

deriveJsonSwagger :: Name -> Name -> Q [Dec]
deriveJsonSwagger typ spec =
    [d|
     instance ToJSON $(conT typ) where
         toJSON = jsonSerializer $(varE spec)
         toEncoding = jsonEncoder $(varE spec)
     instance FromJSON $(conT typ) where
         parseJSON = jsonParser $(varE spec)
     instance ToSchema $(conT typ) where
         declareNamedSchema p = makeDeclareNamedSchema $(varE spec) p
    |]
