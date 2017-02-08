module Main where

import qualified Data.HighJson.THSpec

import Test.Hspec

main :: IO ()
main = hspec $
    do Data.HighJson.THSpec.spec
