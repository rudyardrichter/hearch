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

import Data.Map (Map)

databaseFile :: String
databaseFile = "data/words.db"

-----------------------------------------------------------------------------

-- Format string for the storeFreqMap query.
storeQueryFormat :: Query
storeQueryFormat = "INSERT INTO words (word, page, freq) VALUES (?, ?, ?)"

-- | Store a map of word/page-frequency pairs to the table.
storeFreqMap :: Map String (String, Int) -> IO ()
storeFreqMap freqMap = do
    con <- open databaseFile
    -- TODO: freqMap -> (word, page, freq)
    -- `x :: T` is necessary for Database.SQLite.Simple.ToField
    let word = "testword" :: String
    let page = "testpage" :: String
    let freq = "testfreq" :: String
    execute con storeQueryFormat (word, page, freq)
    close con

-----------------------------------------------------------------------------

-- The structure of the FromRow result which will be extracted from the table.
type GetEntry = (String, String, Int)

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
