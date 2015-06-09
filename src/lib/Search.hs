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
 - (1) Separate the input into individual search terms
 - (2) Retrieve the row results for each search term using getFreqMap
 - (3) For each individual search term:
 -     (A) Apply scorePage to all entries
 -     (B) Aggregate the scores of identical page entries
 -     (C) Standardize the scores
 - (4) Combine the standardized results of all terms into a single list
 -     with concat
 - (5) Aggregate the page scores for the unified collection of search terms
 - (6) Sort the pages by score in descending order
 - (7) Return the n highest-scored pages to the user
 -}
searchFor :: Int -> String -> IO ()
searchFor n = putStrLn . unlines . map (("http://stackoverflow.com" ++) . fst)
            . take n
            . sortBy pageSort
            . aggregate . concat
            . map (normalize . aggregate . map scorePage)
            <=< mapM (getFreqMap databaseFile) . words

-- | Helper function for runSearch. Normalizes a list of (page, score) duples
-- by applying feature scaling. This means that each term of the search gets
-- roughly equal weighting regardless of page hits.
normalize :: [(String, Double)] -> [(String, Double)]
normalize list = map (mapSnd scale) list
  where
    mapSnd f (x, y) = (x, f y)
    scale x = if x == xMin
        then 0.5
        else (x - xMin) / range
    range   = xMax - xMin
    xMax    = maximum snds
    xMin    = minimum snds
    snds    = map snd list

-- | Helper function for runSearch. Converts row entries from the database to
-- a list of (page, score) duples and also drops the word from the results.
scorePage :: (String, String, Int, Int) -> (String, Double)
scorePage (_, page, freq, views) = (page, freq' * logBase 10.0 views')
  where
    freq'  = fromIntegral freq  :: Double
    views' = fromIntegral views :: Double

-- | Helper function for runSearch. In a list of (page, score) entries,
-- it merges entries with the same page and adds their scores together.
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
pageSort :: (Ord b)
         => (a, b) -- (page, score)
         -> (a, b) -- (page, score)
         -> Ordering
pageSort (_, s1) (_, s2) = compare s2 s1
-- (note that the order of pairs in the output is reversed because we want
-- the highest-ranked searches to come first.)
