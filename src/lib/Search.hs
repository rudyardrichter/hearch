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
    rows <- getFreqMap input
    let pageFreqs = dropWord rows
    let topTen = map (("http://stackoverflow.com" ++) . fst)
               . take numberOfResults . sortBy freqSort
               $ pageFreqs
    print topTen

-- Helper function for runSearch. Converts row entries from the database to
-- a list of (page, frequency) pairs.
dropWord :: [(String, String, Int)] -> [(String, Int)]
dropWord = map dropFst
  where
    dropFst (_, y, z) = (y, z)

-- Helper function for runSearch. Sorts (page, frequency) duples in
-- descending order.
freqSort :: (Ord b) => (a, b) -> (a, b) -> Ordering
freqSort (_, b1) (_, b2) = compare b2 b1
-- (note that the order is reversed since we want the highest-ranked searches
-- to come first.)
