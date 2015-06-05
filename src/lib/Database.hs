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
---- Provides functions to use for interfacing with the database
----
-----------------------------------------------------------------------------

module Database where

import Database.SQLite.Simple

import Data.Map (Map)

databaseFile :: String
databaseFile = "data/words.db"

storeQueryFormat :: Query
storeQueryFormat = "INSERT INTO words (word, page, freq) VALUES (?, ?, ?)"

storeFreqMap  :: Map String (String, Int) -> IO ()
storeFreqMap freqMap = do
    con <- open databaseFile
    -- TODO: freqMap -> (word, page, freq)
    -- `x :: T` is necessary for Database.SQLite.Simple.ToField
    let word = "testword" :: String
    let page = "testpage" :: String
    let freq = "testfreq" :: String
    execute con storeQueryFormat (word, page, freq)
    close con

data RetrieveEntry = RetrieveEntry String String Int

getQueryFormat :: Query
getQueryFormat = undefined
