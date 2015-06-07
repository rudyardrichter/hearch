-----------------------------------------------------------------------------
---- |
---- Module      :  Database
---- Copyright   :  (c) Rudyard Richter 2015
---- License     :  MIT
----
---- Maintainer  :  rudyardrichter@uchicago.edu
---- Stability   :  development
---- Portability :  non-portable
----
---- Exports runSearch to Main. runSearch loops continuously, asking the
---- user for input to use for a search, and then printing the top results
---- for that search.
----
-----------------------------------------------------------------------------

module Search (runSearch) where

import Database

import Control.Monad
import Data.List (sortBy)
import System.Exit
import System.IO

-- | The IO loop in which the user performs searches. Essentially a handler
-- for searchFor.
runSearch :: Int -> IO ()
runSearch numberOfResults = forever $ do
    putStr "hearch > "
    -- make sure it prints that ^ string
    hFlush stdout
    -- get input
    input <- getLine
    -- exit if the user wants to
    when (input == "q" || input == "quit") $ do
        putStrLn "Thanks for using Hearch!"
        exitSuccess
    searchFor numberOfResults input

{- |
 - searchFor:
 - (1) separate the input into individual search terms
 - (2) retrieve the row results for each search term
 - (3) remove the search word from the resulting quadruples
 - (4) score each page: (page, freq, views) -> (page, score)
 - (5) aggregate the scores of identical pages into one score
 - (6) sort the pages by score in descending order
 - (7) return the n highest-scored pages to the user
 -}
searchFor :: Int -> String -> IO ()
searchFor n = print . unlines . map (("http://stackoverflow.com" ++) . fst)
            . take n
            . sortBy freqSort
            . aggregate
            . map scorePage
            . concat
            <=< mapM getFreqMap . words

-- Helper function for runSearch. Converts row entries from the database to
-- a list of (page, score) duples and also drops the word from the results.
scorePage :: (String, String, Int, Int) -> (String, Double)
scorePage (_, page, freq, views) = (page, freq' * log views')
  where
    freq'  = fromIntegral freq  :: Double
    views' = fromIntegral views :: Double

aggregate :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
aggregate = loop []
  where
    -- loop goes through each element of rows and applies subLoop to them
    loop acc rows = case rows of
        []     -> acc
        (x:xs) -> loop (subLoop x xs : acc) xs
    -- subLoop checks an element (a, b1) against all other elements of rows
    -- and merges it with elements (a, b2) to produce (a, b1 + b2)
    subLoop acc rows = case rows of
        []     -> acc
        (x:xs) -> subLoop (merge x acc) xs
    merge (a1, b1) (a2, b2) =
        if a1 == a2
            then (a1, b1 + b2)
            else (a1, b1)

-- Helper function for runSearch. Sorts aggregated (page, score) duples in
-- descending order.
freqSort :: (Ord b)
         => (a, b) -- (page, score)
         -> (a, b) -- (page, score)
         -> Ordering
freqSort (_, s1) (_, s2) = compare s2 s1
-- (note that the order of pairs in the output is reversed because we want
-- the highest-ranked searches to come first.)
