module Main where

import qualified Data.JsonSpec

import Test.Hspec

main :: IO ()
main = hspec $
    do Data.JsonSpec.spec
