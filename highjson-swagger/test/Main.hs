module Main where

import qualified Data.HighJson.SwaggerSpec

import Test.Hspec

main :: IO ()
main = hspec $
    do Data.HighJson.SwaggerSpec.spec
