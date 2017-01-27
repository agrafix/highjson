highjson
=====

[![Build Status](https://travis-ci.org/agrafix/highjson.svg)](https://travis-ci.org/agrafix/highjson)

[![Hackage Deps](https://img.shields.io/hackage-deps/v/highjson.svg)](http://packdeps.haskellers.com/reverse/highjson)

## Intro

Hackage: [highjson](http://hackage.haskell.org/package/highjson)

 **WARNING: Work in progress!**

Low boilerplate, easy to use and very fast Haskell JSON serialisation and
parsing without the help of TemplateHaskell or Generics built on top of [aeson](http://hackage.haskell.org/package/aeson).

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

someDummySpec =
    JsonSpec SomeDummy $
    "int" .= sd_int
    :+: "bool" .= sd_bool
    :+: "text" .= sd_text
    :+: "maybe" .=? sd_maybe
    :+: EmptySpec

instance ToJSON SomeDummy where
    toJSON = makeSerialiser someDummySpec

instance FromJSON SomeDummy where
    parseJSON = makeParser someDummySpec

test =
    decodeEither "{\"int\": 34, \"text\": \"Teext\", \"bool\": true}"
        == Right (SomeDummy 34 True "Teext" Nothing)
```

For more usage examples check the tests.

## Install

* Using cabal: `cabal install highjson`
* From Source: `git clone https://github.com/agrafix/highjson.git && cd highjson && cabal install`

## Todo

* Generate Swagger / TypeScript / Elm interfaces from object specs
* Write more tests (always a good idea)
* ...

## Benchmarks

Current results on my MacBook Pro:

```
benchmarking twitter/aeson/generic
time                 2.512 ms   (2.473 ms .. 2.554 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 2.494 ms   (2.452 ms .. 2.535 ms)
std dev              137.8 μs   (112.7 μs .. 163.9 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking twitter/aeson/highjson
time                 2.024 ms   (1.980 ms .. 2.068 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 2.020 ms   (1.990 ms .. 2.047 ms)
std dev              98.81 μs   (84.63 μs .. 120.6 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking twitter-jp/aeson/generic
time                 2.760 ms   (2.691 ms .. 2.842 ms)
                     0.995 R²   (0.992 R² .. 0.997 R²)
mean                 2.795 ms   (2.754 ms .. 2.857 ms)
std dev              169.6 μs   (131.0 μs .. 251.8 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking twitter-jp/aeson/highjson
time                 2.250 ms   (2.202 ms .. 2.297 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 2.227 ms   (2.193 ms .. 2.266 ms)
std dev              112.6 μs   (89.98 μs .. 155.8 μs)
variance introduced by outliers: 35% (moderately inflated)
```

The benchmarks are derived from [aeson](https://github.com/bos/aeson)'s
twitter-json-parsing benchmarks.
