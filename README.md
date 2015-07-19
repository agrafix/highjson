highjson
=====

[![Build Status](https://travis-ci.org/agrafix/highjson.svg)](https://travis-ci.org/agrafix/highjson)

[![Hackage Deps](https://img.shields.io/hackage-deps/v/highjson.svg)](http://packdeps.haskellers.com/reverse/highjson)

## Intro

Hackage: [highjson](http://hackage.haskell.org/package/highjson)

Low boilerplate, easy to use and very fast Haskell JSON parsing. **WARNING: Work in progress!**

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Json.Parser

data SomeDummy
   = SomeDummy
   { sd_int :: Int
   , sd_bool :: Bool
   , sd_text :: T.Text
   , sd_either :: Either Bool T.Text
   , sd_maybe :: Maybe Int
   } deriving (Show, Eq)

instance JsonReadable SomeDummy where
    readJson =
       runSpec SomeDummy $ "int" :&&: "bool" :&&: "text" :&&: "either" :&&: "maybe" :&&: ObjSpecNil

test =
    parseJsonBs "{\"int\": 34, \"text\": \"Teext\", \"bool\": true, \"either\": false}"
                               == Right (SomeDummy 34 True "Teext" (Left False) Nothing)
```

## Install

* Using cabal: `cabal install highjson`
* From Source: `git clone https://github.com/agrafix/highjson.git && cd highjson && cabal install`

## Todo

* Implement proper string parsing (handle escape charaters)
* Write more tests
* Allow fast json generation via object specs
* Generate typescript interfaces from object specs
* ...

## Benchmarks

To run the benchmarks, use `cabal bench`. Current results on my MacBook Pro:

```
$ cabal bench
Preprocessing library highjson-0.1.0.0...
[1 of 1] Compiling Data.Json.Parser ( src/Data/Json/Parser.hs, dist/build/Data/Json/Parser.o )
In-place registering highjson-0.1.0.0...
Preprocessing benchmark 'highjson-benchmarks' for highjson-0.1.0.0...
[1 of 1] Compiling Main             ( bench/Twitter.hs, dist/build/highjson-benchmarks/highjson-benchmarks-tmp/Main.o ) [Data.Json.Parser changed]
Linking dist/build/highjson-benchmarks/highjson-benchmarks ...
Running 1 benchmarks...
Benchmark highjson-benchmarks: RUNNING...
benchmarking twitter/aeson
time                 2.200 ms   (2.044 ms .. 2.349 ms)
                     0.983 R²   (0.975 R² .. 0.996 R²)
mean                 2.065 ms   (2.031 ms .. 2.120 ms)
std dev              138.3 μs   (93.40 μs .. 204.8 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarking twitter/highjson
time                 2.058 ms   (2.032 ms .. 2.087 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 2.077 ms   (2.050 ms .. 2.115 ms)
std dev              103.6 μs   (80.86 μs .. 138.0 μs)
variance introduced by outliers: 35% (moderately inflated)
```
