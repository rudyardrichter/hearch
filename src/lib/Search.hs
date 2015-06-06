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
---- Provides functions to use for interfacing with the Redis database
---- (via hedis, the Haskell Redis client).
----
-----------------------------------------------------------------------------

module Search (runSearch) where

import Database

import Control.Monad
import Data.List (sortBy)

runSearch :: IO ()
runSearch = forever $ do
    putStr "hearch > "
    input <- getLine
    -- exit if the user wants to
    when (input == "q" || input == "quit") $ do
        putStrLn "Thanks for using Hearch!"
        return ()
    rows <- getFreqMap input
    let pageFreqs = dropWord rows
    let topTen = map (("http://stackoverflow.com" ++) . fst)
               . take 10 . sortBy freqSort
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
freqSort (a1, b1) (a2, b2) = compare b2 b1
