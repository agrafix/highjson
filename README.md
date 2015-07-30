highjson
=====

[![Build Status](https://travis-ci.org/agrafix/highjson.svg)](https://travis-ci.org/agrafix/highjson)

[![Hackage Deps](https://img.shields.io/hackage-deps/v/highjson.svg)](http://packdeps.haskellers.com/reverse/highjson)

## Intro

Hackage: [highjson](http://hackage.haskell.org/package/highjson)

 **WARNING: Work in progress!**

Low boilerplate, easy to use and very fast Haskell JSON serialisation and
parsing without the help of TemplateHaskell or Generics. Fast parsing is
achieved by trying to avoid intermediate data structures and c string
decoding, fast serialisation is powered by [buffer-builder](https://github.com/chadaustin/buffer-builder/).

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

* Generate typescript / Elm interfaces from object specs
* Write more tests (always a good idea)
* ...

## Benchmarks

To run the benchmarks, use `cabal bench`. Current results on my MacBook Pro:

```
$ cabal bench
Preprocessing library highjson-0.2.0.3...
In-place registering highjson-0.2.0.3...
Preprocessing benchmark 'highjson-benchmarks' for highjson-0.2.0.3...
Running 1 benchmarks...
Benchmark highjson-benchmarks: RUNNING...
benchmarking twitter/aeson
time                 2.118 ms   (2.075 ms .. 2.166 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 2.099 ms   (2.068 ms .. 2.145 ms)
std dev              130.3 μs   (88.79 μs .. 238.0 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking twitter/highjson
time                 2.096 ms   (2.043 ms .. 2.147 ms)
                     0.995 R²   (0.992 R² .. 0.997 R²)
mean                 2.049 ms   (2.016 ms .. 2.089 ms)
std dev              121.4 μs   (98.95 μs .. 168.9 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking twitter-jp/aeson
time                 2.513 ms   (2.443 ms .. 2.597 ms)
                     0.993 R²   (0.989 R² .. 0.998 R²)
mean                 2.501 ms   (2.468 ms .. 2.542 ms)
std dev              129.0 μs   (108.7 μs .. 158.0 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking twitter-jp/highjson
time                 2.415 ms   (2.359 ms .. 2.487 ms)
                     0.994 R²   (0.992 R² .. 0.997 R²)
mean                 2.383 ms   (2.347 ms .. 2.427 ms)
std dev              132.1 μs   (99.37 μs .. 168.4 μs)
variance introduced by outliers: 39% (moderately inflated)
```

The benchmarks are derived from [aeson](https://github.com/bos/aeson)'s
twitter-json-parsing benchmarks and should probably move there when this library is in a
more complete state.
