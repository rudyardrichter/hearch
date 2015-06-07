-----------------------------------------------------------------------------
---- |
---- Module      :  Test.Main
---- Copyright   :  (c) Rudyard Richter 2015
---- License     :  MIT
----
---- Maintainer  :  rudyardrichter@uchicago.edu
---- Stability   :  development
---- Portability :  non-portable
----
---- The test suite.
----
-----------------------------------------------------------------------------

module Main (main) where

import Search

import Data.List (sort)

import Test.Hspec

main :: IO ()
main = hspec $ describe "Testing Hearch" $ do

    describe "module Crawler" $ do
        it "needs more tests" $ do
            pending

    describe "module Database" $ do
        it "needs more tests" $ do
            pending

    describe "module Search" $ do
        -- note that aggregate is allowed to change the order of the
        -- elements, since searchFor hits the result with
        -- sortBy freqSort immediately afterwards
        describe "function aggregate" $ do
            it "should do nothing if there are no redundancies" $ do
                sort (aggregate [("a", 1), ("b", 2), ("c", 3)])
                `shouldBe` [("a", 1), ("b", 2), ("c", 3)]
            it "should aggregate redundant elements" $ do
                sort (aggregate [("a", 1), ("a", 10), ("a", 100)])
                `shouldBe` [("a", 111)]
            it "should combine redundant elements" $ do
                sort (aggregate [("a", 1), ("b", 2), ("c", 3), ("a", 10), ("b", 20), ("a", 100)])
                `shouldBe` [("a", 111), ("b", 22), ("c", 3)]
