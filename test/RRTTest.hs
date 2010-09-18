{-# LANGUAGE TemplateHaskell #-}

module RRTTest (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck.Property ((==>))

import Data.MetricSpace
import Data.RRT


tests :: Test
tests = $(testGroupGenerator)


prop_num_streams_eq_num_nodes stream = length tree == length stream
    where config = Config (controlEdgeProp (+)) [0] distance
          tree = maybeSearch config stream
          types = (stream :: [(Integer, [Integer])])


prop_failed_means_empty_tree stream = null tree
    where config = Config (const $ const Nothing) [0] distance
          tree = search config stream
          types = (stream :: [(Integer, [Integer])])
