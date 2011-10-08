module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.API

import Validation

main :: IO ()
main = defaultMain [buildTest getValidationTests]