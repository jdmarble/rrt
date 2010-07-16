module Main where

import Test.Framework (defaultMain)

import qualified RRTTest

main :: IO ()
main = defaultMain
       [ RRTTest.tests
       ]
