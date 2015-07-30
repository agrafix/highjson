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
time                 2.080 ms   (2.038 ms .. 2.123 ms)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 2.086 ms   (2.058 ms .. 2.116 ms)
std dev              100.5 μs   (86.53 μs .. 117.8 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking twitter/highjson
time                 2.050 ms   (2.000 ms .. 2.099 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 2.043 ms   (2.017 ms .. 2.092 ms)
std dev              120.0 μs   (75.03 μs .. 202.7 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking twitter-jp/aeson
time                 2.494 ms   (2.447 ms .. 2.547 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 2.486 ms   (2.454 ms .. 2.528 ms)
std dev              119.5 μs   (97.64 μs .. 149.2 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking twitter-jp/highjson
time                 2.355 ms   (2.318 ms .. 2.394 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 2.360 ms   (2.331 ms .. 2.392 ms)
std dev              101.7 μs   (79.40 μs .. 147.6 μs)
variance introduced by outliers: 27% (moderately inflated)

Benchmark highjson-benchmarks: FINISH
```

The benchmarks are derived from [aeson](https://github.com/bos/aeson)'s
twitter-json-parsing benchmarks and should probably move there when this library is in a
more complete state.
