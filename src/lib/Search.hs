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
    let topTen = unlines
               . map (("http://stackoverflow.com" ++) . fst')
               . take numberOfResults . sortBy freqSort
               $ pageFreqs
    print topTen
  where
    fst' (a, _, _) = a

-- Helper function for runSearch. Converts row entries from the database to
-- a list of (page, frequency, views) triples.
dropWord :: [(String, String, Int, Int)] -> [(String, Int, Int)]
dropWord = map dropFst
  where
    dropFst (_, x, y, z) = (x, y, z)

-- Helper function for runSearch. Sorts (page, frequency, views) triples in
-- descending order.
freqSort :: (Integral b, Integral c)
         => (a, b, c) -- (_, frequency, views)
         -> (a, b, c) -- (_, frequency, views)
         -> Ordering
freqSort (_, f1, v1) (_, f2, v2) = compare (f2' * log v2') (f1' * log v1')
  where
    f1' = fromIntegral f1 :: Double
    v1' = fromIntegral v1 :: Double
    f2' = fromIntegral f2 :: Double
    v2' = fromIntegral v2 :: Double
-- (note that the order of pairs in the output is reversed because we want
-- the highest-ranked searches to come first.)
