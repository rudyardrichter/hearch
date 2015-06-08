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

module Search where

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
 - (2) retrieve the row results for each search term using getFreqMap
 - (3) remove the search word from the resulting quadruples
 - (4) score each page: (page, freq, views) -> (page, score)
 - (5) aggregate the scores of identical pages into one score
 - (6) sort the pages by score in descending order
 - (7) return the n highest-scored pages to the user
 -}
searchFor :: Int -> String -> IO ()
searchFor n = putStrLn . unlines . map (("http://stackoverflow.com" ++) . fst)
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

-- Combine page entries.
aggregate :: [(String, Double)] -> [(String, Double)]
aggregate = loop []
  where
    loop acc rows = case rows of
        []     -> acc
        (x:xs) ->
            let (x', xs') = foldMerge x [] xs
            in loop (x' : acc) xs'
    foldMerge merged unmerged toMerge = case toMerge of
        []     -> (merged, unmerged)
        (x:xs) -> if fst x == fst merged
            then foldMerge (fst merged, snd merged + snd x) unmerged xs
            else foldMerge merged (x : unmerged) xs

-- Helper function for runSearch. Sorts aggregated (page, score) duples in
-- descending order.
freqSort :: (Ord b)
         => (a, b) -- (page, score)
         -> (a, b) -- (page, score)
         -> Ordering
freqSort (_, s1) (_, s2) = compare s2 s1
-- (note that the order of pairs in the output is reversed because we want
-- the highest-ranked searches to come first.)
