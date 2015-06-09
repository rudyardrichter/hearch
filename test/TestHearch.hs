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

import Data.List (nub, sort)
import Text.HTML.TagSoup

import qualified Data.Map as Map (fromList)

import qualified Data.Set as Set (member)

import Test.Hspec
import Test.QuickCheck

-----------------------------------------------------------------------------

-- Globals for use in the tests.

testDBFile :: String
testDBFile = "data/testWords.db"

testURLFile :: String
testURLFile = "data/testURLs.db"

testCrawledFile :: String
testCrawledFile = "data/testCrawled.db"

testPage0 :: String
testPage0 = "http://stackoverflow.com/questions/1012573/getting-started-with-haskell"

testPage0Body :: [String]
testPage0Body = words $ "For a few days Ive tried to wrap my head around the functional programming paradigm"

testPage0Links :: [String]
testPage0Links = ["/questions"
                 ,"/questions/ask"
                 ,"/questions/1012573/getting-started-with-haskell"
                 ,"/questions/tagged/haskell"
                 ,"/questions/tagged/functional-programming"]

testPage1 :: String
testPage1 = "http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags"

testPage2 :: String
testPage2 = "http://stackoverflow.com/questions/1642028/what-is-the-name-of-the-operator"

testPage2Title :: String
testPage2Title = "c++ - What is the name of the \"-->\" operator? - Stack Overflow"
-- the front of the HTML of testPage1; for testing Crawler.httpRequest
testPage1HTML :: String
testPage1HTML = "<!DOCTYPE html>\r\n<html itemscope itemtype=\"http://schema.org/QAPage\">"

-- from robots.txt; for testing Crawler.correctDomain
disallowed :: [String]
disallowed = ["/questions/*answertab=*"
             ,"/questions/tagged/***"
             ,"/questions/tagged/*%20*"
             ,"/questions/*/answer/submit"]

-- from data/ignore.txt; for testing Crawler.readFileToSet
testIgnore :: [String]
testIgnore = ["because", "think", "would", "just", "is", "a"]

-----------------------------------------------------------------------------

main :: IO ()
main = hspec $ describe "Testing Hearch" $ do

    describe "module Crawler" $ do
        describe "function readFileToSet" $ do
            it "should load the lines of a file into a Set structure" $ do
                testSet <- readFileToSet "data/ignore.txt"
                let ret = return . and . map (flip Set.member testSet) $ testIgnore
                ret `shouldReturn` True
        describe "function httpRequest" $ do
            it "should retrieve the raw HTML content of a page" $ do
                fmap (take 69) (httpRequest testPage1)
                `shouldReturn` testPage1HTML
        describe "function getTitle" $ do
            it "should retrieve the title of a page" $ do
                tags <- tagsRequest testPage2
                let ret = return $ getTitle tags
                ret `shouldReturn` testPage2Title
        describe "function getBody" $ do
            it "should retrieve the words in the body of a page" $ do
                tags <- tagsRequest testPage0
                let ret = return . take 15 . getBody $ tags
                ret `shouldReturn` testPage0Body
        describe "function getLinks" $ do
            it "should retrieve the links from a page" $ do
                tags <- tagsRequest testPage0
                let ret = return . take 5
                        . filter (beginsWith "/questions") . getLinks $ tags
                ret `shouldReturn` testPage0Links
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
            it "should return a frequency map from the required arguments" $ do
                makeWordFreqMap 5 "/z" ["a","a","a","a","b"
                                       ,"b","b","c","c","d"]
                `shouldBe`
                Map.fromList [("a", ("/z", 4, 5))
                             ,("b", ("/z", 3, 5))
                             ,("c", ("/z", 2, 5))
                             ,("d", ("/z", 1, 5))]
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
        describe "function storeFreqMap, function getFreqMap" $ do
            it "should store/get freqMap entries from the table" $ do
                storeFreqMap testDBFile $
                    Map.fromList [("this is", ("a test", 10, 4))]
                let ret = getFreqMap testDBFile "this is"
                ret `shouldReturn` [("this is", "a test", 10, 4)]
        describe "function joinAssosc" $ do
            it "should join (a, (x, y, z)) into (a, x, y, z)" $
                property $ \(a, (b, c, d)) ->
                    joinAssosc (a, (b, c, d)) == (a, b, c, d)
                -- joinAssosc ("a", ("b", 3, 4)) `shouldBe` ("a", "b", 3, 4)
        describe "function getURL" $ do
            it "retrieves a URL to crawl from the URL table" $ do
                getURL testURLFile `shouldReturn` "/questions/1012573/getting-started-with-haskell"
        describe "function addURL" $ do
            it "adds a URL to crawl to the URL table" $ do
                addURL testURLFile "/testing-addURL"
                getURL testURLFile `shouldReturn` "/testing-addURL"
        describe "function addCrawledURL" $ do
            it "adds a URL to the crawled table" $ do
                addCrawledURL testCrawledFile "/testing-addCrawledURL"
                let ret = wasNotCrawled testCrawledFile "/testing-addCrawledURL"
                ret `shouldReturn` False
        describe "function wasNotCrawled" $ do
            it "should return True for links not in the table" $ do
                wasNotCrawled testCrawledFile "/wrongURL" `shouldReturn` True
            it "should return False for links in the table" $ do
                wasNotCrawled testCrawledFile "/test-url" `shouldReturn` False

    describe "module Search" $ do
        describe "function normalize" $ do
            it "should always return values between 0 and 1" $
                property $ \xs ->
                    and . map (\(_,x) -> 0 <= x && x <= 1) $ normalize xs
        -- Note that aggregate is allowed to change the order of the
        -- elements, since searchFor hits the result with sortBy freqSort
        -- immediately afterwards. Also note that QuickCheck, in all its
        -- wonder, has instance Arbitrary [(a, b)].
        describe "function aggregate" $ do
            it "should do nothing if there are no redundancies" $
                property $ \xs ->
                    map fst xs == nub (map fst xs)
                        ==> sort (aggregate xs) == sort xs
            it "should aggregate redundant elements" $ do
                sort (aggregate [("a", 1), ("a", 10), ("a", 100)])
                `shouldBe` [("a", 111)]
            it "should combine redundant elements" $ do
                sort (aggregate [("a", 1), ("b", 2), ("c", 3), ("a", 10), ("b", 20), ("a", 100)])
                `shouldBe` [("a", 111), ("b", 22), ("c", 3)]
        -- If anything fails in either scorePage or pageSort then something
        -- has already gone catastrophically wrong.
