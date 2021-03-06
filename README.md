highjson
=====

[![Hackage Deps](https://img.shields.io/hackage-deps/v/highjson.svg)](http://packdeps.haskellers.com/reverse/highjson)

## Intro

Hackage: [highjson](http://hackage.haskell.org/package/highjson)

Low boilerplate, easy to use and very fast Haskell JSON serialisation and
parsing without the help of TemplateHaskell or Generics built on top of [aeson](http://hackage.haskell.org/package/aeson). The optional package `highjson-swagger` will also help automatically generating a OpenAPI schema.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Data.Aeson
import Data.HighJson

import Data.HighJson.Swagger -- optional
import Data.Swagger -- optional

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_either :: Either Bool T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

someDummySpec :: RecordTypeSpec SomeDummy _
someDummySpec =
    recSpec "Some Dummy" Nothing SomeDummy $
    "int" .= sd_int
    :& "bool" .= sd_bool
    :& "text" .= sd_text
    :& "either" .= sd_either
    :& "maybe" .= sd_maybe

instance ToJSON SomeDummy where
    toJSON = jsonSerializer someDummySpec
    toEncoding = jsonEncoder someDummySpec

instance FromJSON SomeDummy where
    parseJSON = jsonParser someDummySpec

instance ToSchema SomeSum where -- optional, generates swagger2 specifications
    declareNamedSchema p = makeDeclareNamedSchema someDummySpec p

test =
    decodeEither "{\"int\": 34, \"text\": \"Teext\", \"bool\": true}"
        == Right (SomeDummy 34 True "Teext" Nothing)
```

### Template haskell

There's also a small shortcut via template haskell (`highjson-th`):

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.HighJson
import Data.HighJson.Swagger
import Data.HighJson.TH

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_either :: Either Bool T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

someDummySpec :: RecordTypeSpec SomeDummy _
someDummySpec =
    recSpec "Some Dummy" Nothing SomeDummy $
    "int" .= sd_int
    :& "bool" .= sd_bool
    :& "text" .= sd_text
    :& "either" .= sd_either
    :& "maybe" .= sd_maybe

$(deriveJsonSwagger ''SomeDummy 'someDummySpec)

test =
    decodeEither "{\"int\": 34, \"text\": \"Teext\", \"bool\": true}"
        == Right (SomeDummy 34 True "Teext" Nothing)
```

### Tests

For more usage examples check the tests.

## Install

* Using cabal: `cabal install highjson`
* From Source: `git clone https://github.com/agrafix/highjson.git && cd highjson && cabal install`

## Benchmarks

There are benchmarks in the project and it is expected to be en par or faster than `aeson`s generic instances.
