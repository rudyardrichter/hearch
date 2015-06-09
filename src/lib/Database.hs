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

-----------------------------------------------------------------------------

-- Functions for storing entries in the database.

databaseFile :: String
databaseFile = "data/words.db"

-- Format string for the storeFreqMap query.
storeQueryFormat :: Query
storeQueryFormat = "INSERT INTO words (word, page, freq, views) VALUES (?, ?, ?, ?)"

-- default: databaseFile
-- | Store a word/page-frequency-views map to the table.
storeFreqMap :: String -> Map String (String, Int, Int) -> IO ()
storeFreqMap file freqMap = withConnection file $ \con -> do
    -- Map Word (Page, Freq, Views) -> (Word, Page, Freq, Views)
    let rows = map joinAssosc $ Map.assocs freqMap
    -- store all the rows in the table
    forM_ rows $ execute con storeQueryFormat

-- Helper function for storeFreqMap. Joins a (word, (page, freq, views))
-- map into one quadruple which can then be stored in the table.
joinAssosc :: (String, (String, Int, Int)) -> (String, String, Int, Int)
joinAssosc (a, (b, c, d)) = (a, b, c, d)

-----------------------------------------------------------------------------

-- getFreqMap: retrieve an entry from the database.

-- | The structure of the FromRow result which will be extracted from the table.
-- type GetEntry = (word, page, frequency, views)
type GetEntry = (String, String, Int, Int)

-- GetEntry will have a built-in FromRow instance in Database.SQLite.Simple,
-- since it is just a normal triple, so it is not required that we build a
-- new FromRow instance for it.

-- Format string for the getFreqMap query.
getQueryFormat :: Query
getQueryFormat = "SELECT * FROM words WHERE word = ?"

-- default: databaseFile
-- | Query to retrieve all rows containing a given word.
getFreqMap :: String -> String -> IO [GetEntry]
getFreqMap file word = withConnection file $ \con -> do
    rows <- (query con getQueryFormat (Only (word :: String))) :: IO [GetEntry]
    return rows

-----------------------------------------------------------------------------

-- Functions for adding, retrieving, and deleting URLs to/from the URL table.

urlsFile :: String
urlsFile = "data/urls.db"

-- | The type of a URL retrieval from the table.
type GetURL = Only String

-- Format string for adding a URL to the URL table. Requires one additional
-- argument.
addURLFormat :: Query
addURLFormat = "INSERT INTO urls (url) VALUES (?)"

-- | Add a new URL to the table of URLs to be crawled.
addURL :: String -> String -> IO ()
addURL file url = withConnection file $ \con -> do
    -- we put [url] since execute wants list of parameters
    execute con addURLFormat [url]

-- default: urlsFile
-- | Add a list of URLs to the table of URLs to be crawled.
addURLs :: String -> [String] -> IO ()
addURLs file urls = withConnection file $ \con -> do
    mapM_ (execute con addURLFormat . (:[])) urls

-- Format string for the getURL retrieval query. Note that it requires no
-- additional arguments. Picks a new URL at random (using SQLite's random()).
getURLFormat :: Query
getURLFormat = "SELECT * FROM urls ORDER BY RANDOM() LIMIT 1"

-- Format string for the getURL deletion query. Requires the URL to delete
-- as a formatting argument.
removeURLFormat :: Query
removeURLFormat = "DELETE FROM urls WHERE url = ?"

-- default: urlsFile
-- | Retrieve and delete a (random) URL from the URL table. Has the added
-- benefit of providing another layer of redundancy to prevent re-crawling
-- a URL which has already been crawled.
getURL :: String -> IO String
getURL file = withConnection file $ \con -> do
    -- getURLFormat does not need formatting arguments
    row <- (query_ con getURLFormat) :: IO [GetURL]
    let url = fromOnly . head $ row
    execute con removeURLFormat [url]
    return url

-----------------------------------------------------------------------------

-- Functions for adding URLs to the crawled table and checking if a URL is in
-- the table.

crawledFile :: String
crawledFile = "data/crawled.db"

-- Format string for addCrawledURL.
addCrawledFormat :: Query
addCrawledFormat = "INSERT INTO crawled (url) VALUES (?)"

-- Note that we do not need a function to remove URLs from the crawled table,
-- as this never needs to be done.

-- default: crawledFile
-- | Add a URL to the crawled table.
addCrawledURL :: String -> String -> IO ()
addCrawledURL file url = withConnection file $ \con -> do
    -- we write [url] because execute expects a list of parameters
    execute con addCrawledFormat [url]

wasCrawledFormat :: Query
wasCrawledFormat = "SELECT EXISTS(SELECT 1 FROM crawled WHERE url = ? LIMIT 1)"

-- default: crawledFile
-- | Check if the URL is entered in the crawled table (i.e. has already been
-- crawled).
wasNotCrawled :: String -> String -> IO Bool
wasNotCrawled file url = withConnection file $ \con -> do
    -- query also expects a list of paramaters, so we put [url] like above
    row <- (query con wasCrawledFormat (Only url)) :: IO [Only Int]
    if null row
        then return True
        else return . (== 0) . fromOnly . head $ row
