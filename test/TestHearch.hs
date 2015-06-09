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

import Data.Map (Map)
import qualified Data.Map as Map (fromList)

import Test.Hspec

testPage0 :: String
testPage0 = "http://stackoverflow.com/questions/1012573/getting-started-with-haskell"

testPage1 :: String
testPage1 = "http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags"

-- from robots.txt; for testing Crawler.correctDomain
disallowed :: [String]
disallowed = ["/questions/*answertab=*"
             ,"/questions/tagged/***"
             ,"/questions/tagged/*%20*"
             ,"/questions/*/answer/submit"]

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
            it "should return 2 if it fails to retrieve a view count" $ do
                httpRequest "http://google.com" >>= getViews . parseTags
                `shouldReturn` 2
        describe "function makeWordFreqMap" $ do
            it "should work" $ do
                makeWordFreqMap 5 "/z" ["a","a","a","a","b"
                                       ,"b","b","c","c","d"]
                `shouldBe`
                Map.fromList [("a", ("/z", 4, 5))
                             ,("b", ("/z", 3, 5))
                             ,("c", ("/z", 2, 5))
                             ,("d", ("/z", 1, 5))]
            it "should run the test case provided in Crawler" $ do
                pendingWith "TODO"
        describe "function correctDomain" $ do
            it "should disallow URLs not beginning with \"questions\"" $ do
                correctDomain "/q" `shouldBe` False
                correctDomain "/users" `shouldBe` False
            it "should allow valid links" $ do
                correctDomain "/questions/23475890237489" `shouldBe` True
                correctDomain "/questions/$%%$^$%&@#^^&*" `shouldBe` True
            it "should disallow domains specified in robots.txt" $ do
                or (map correctDomain disallowed) `shouldBe` False
        describe "function beginsWith" $ do
            it "should return true for null prefix" $ do
                beginsWith "" "quick brown fox" `shouldBe` True
            it "should return false for null string, non-null prefix" $ do
                beginsWith "quick brown fox" "" `shouldBe` False
            it "should correctly identify the prefix of a string" $ do
                beginsWith "quick" "quick brown fox" `shouldBe` True
            it "should not match on a string which is not a prefix" $ do
                beginsWith "quik" "quick brown fox" `shouldBe` False
        describe "function formatLinks" $ do
            it "should format links" $ do
                formatLinks ["", "/questions", "/users", "/potatoes"]
                `shouldBe` ["http://stackoverflow.com"
                           ,"http://stackoverflow.com/questions"
                           ,"http://stackoverflow.com/users"
                           ,"http://stackoverflow.com/potatoes"]

    describe "module Database" $ do
        describe "function joinAssosc" $ do
            it "should join (a, (x, y, z)) into (a, x, y, z)" $ do
                joinAssosc ("a", ("b", 3, 4)) `shouldBe` ("a", "b", 3, 4)
        describe "function wasNotCrawled" $ do
            it "should return True for links not in the table" $ do
                wasNotCrawled "/wrongURL" `shouldReturn` True
        describe "function ?" $ do
            it "should have more tests" $ do
                pendingWith "TODO"

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
