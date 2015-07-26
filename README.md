highjson
=====

[![Build Status](https://travis-ci.org/agrafix/highjson.svg)](https://travis-ci.org/agrafix/highjson)

[![Hackage Deps](https://img.shields.io/hackage-deps/v/highjson.svg)](http://packdeps.haskellers.com/reverse/highjson)

## Intro

Hackage: [highjson](http://hackage.haskell.org/package/highjson)

Low boilerplate, easy to use and very fast Haskell JSON serialisation and parsing. **WARNING: Work in progress!**

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_either :: Either Bool T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

someDummySpec =
    JsonSpec SomeDummy $
    "int" .= sd_int
    :+: "bool" .= sd_bool
    :+: "text" .= sd_text
    :+: "either" .= sd_either
    :+: "maybe" .=? sd_maybe
    :+: EmptySpec

instance ToJson SomeDummy where
    toJson = makeSerialiser someDummySpec

instance JsonReadable SomeDummy where
    readJson = makeParser someDummySpec

test =
    parseJsonBs "{\"int\": 34, \"text\": \"Teext\", \"bool\": true, \"either\": false}"
        == Right (SomeDummy 34 True "Teext" (Left False) Nothing)
 ```

For more usage examples check the tests.

## Install

* Using cabal: `cabal install highjson`
* From Source: `git clone https://github.com/agrafix/highjson.git && cd highjson && cabal install`

## Todo

* Implement proper string parsing (handle escape charaters)
* Write more tests
* Generate typescript interfaces from object specs
* ...

## Benchmarks

To run the benchmarks, use `cabal bench`. Current results on my MacBook Pro:

```
$ cabal bench
Preprocessing library highjson-0.2.0.0...
In-place registering highjson-0.2.0.0...
Preprocessing benchmark 'highjson-benchmarks' for highjson-0.2.0.0...
Running 1 benchmarks...
Benchmark highjson-benchmarks: RUNNING...
benchmarking twitter/aeson
time                 2.148 ms   (2.102 ms .. 2.187 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 2.137 ms   (2.112 ms .. 2.169 ms)
std dev              99.12 μs   (81.84 μs .. 120.1 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking twitter/highjson
time                 2.196 ms   (2.162 ms .. 2.235 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 2.222 ms   (2.195 ms .. 2.252 ms)
std dev              94.52 μs   (75.45 μs .. 125.1 μs)
variance introduced by outliers: 28% (moderately inflated)
```

The benchmarks are derived from [aeson](https://github.com/bos/aeson)'s
twitter-json-parsing benchmarks and should probably more there when this library is in a
more complete state.
