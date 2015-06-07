{-# LANGUAGE OverloadedStrings #-}

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
---- Provides functions to use for interfacing with the database.
----
-----------------------------------------------------------------------------

module Database where

import Database.SQLite.Simple

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

databaseFile :: String
databaseFile = "data/words.db"

-----------------------------------------------------------------------------

-- Format string for the storeFreqMap query.
storeQueryFormat :: Query
storeQueryFormat = "INSERT INTO words (word, page, freq, views) VALUES (?, ?, ?, ?)"

-- | Store a word/page-frequency-views map to the table.
storeFreqMap :: Map String (String, Int, Int) -> IO ()
storeFreqMap freqMap = do
    con <- open databaseFile
    -- Map Word (Page, Freq, Views) -> (Word, Page, Freq, Views)
    let rows = map joinAssosc $ Map.assocs freqMap
    -- store all the rows in the table
    forM_ rows $ execute con storeQueryFormat
    close con

-- Helper function for storeFreqMap. Joins a (word, (page, freq, views))
-- map into one quadruple which can then be stored in the table.
joinAssosc :: (String, (String, Int, Int)) -> (String, String, Int, Int)
joinAssosc (a, (b, c, d)) = (a, b, c, d)

-----------------------------------------------------------------------------

-- The structure of the FromRow result which will be extracted from the table.
-- type GetEntry = (word, page, frequency, views)
type GetEntry = (String, String, Int, Int)

-- GetEntry will have a built-in FromRow instance in Database.SQLite.Simple,
-- since it is just a normal triple, so it is not required that we build a
-- new FromRow instance for it.

-- Format string for the getFreqMap query.
getQueryFormat :: Query
getQueryFormat = "SELECT * FROM words WHERE word = ?"

-- | Query to retrieve all rows containing a given word.
getFreqMap :: String -> IO [GetEntry]
getFreqMap word = do
    con <- open databaseFile
    rows <- (query con getQueryFormat (Only (word :: String))) :: IO [GetEntry]
    close con
    return rows
