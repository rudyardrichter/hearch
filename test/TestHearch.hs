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

import Crawler
import Database
import Search

import Data.List (sort)
import Text.HTML.TagSoup

import Test.Hspec

testPage0 :: String
testPage0 = "http://stackoverflow.com/questions/1012573/getting-started-with-haskell"

testPage1 :: String
testPage1 = "http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags"

main :: IO ()
main = hspec $ describe "Testing Hearch" $ do

    describe "module Crawler" $ do
        describe "function getViews" $ do
            it "should return the view count of a Stack Overflow page" $ do
                httpRequest testPage0 >>= getViews . parseTags
                                      >>= return . (>= 131823)
                `shouldReturn` True
            it "should return the view count of arbitrary Stack Overflow pages" $ do
                httpRequest testPage1 >>= getViews . parseTags
                                      >>= return . (>= 1320684)
                `shouldReturn` True
            it "should return 1 if it fails to retrieve a view count" $ do
                httpRequest "http://google.com" >>= getViews . parseTags
                `shouldReturn` 1

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
