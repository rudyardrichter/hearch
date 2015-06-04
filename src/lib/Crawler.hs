{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
---- |
---- Module      :  Crawler
---- Copyright   :  (c) Rudyard Richter 2015
---- License     :  MIT
----
---- Maintainer  :  rudyardrichter@uchicago.edu
---- Stability   :  development
---- Portability :  non-portable
----
---- A web crawler to gather word counts from pages, store them in the
---- database, and automatically traverse the pages at other hyperlinks
---- gathered from each page.
----
-----------------------------------------------------------------------------

module Crawler (runCrawler) where

import Database

import Control.Monad
import Control.Exception
import Data.Char
import Data.Typeable
import Database.Redis
import Network.HTTP hiding (Connection)
import System.IO
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as BC

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

type URL = String

defaultURLFile :: FilePath
defaultURLFile = "data/urls.txt"

-- read file containing list of URLs to crawl
readURLFile :: FilePath -> IO [URL]
readURLFile = fmap lines . readFile

-- write new URLs to the end of the URL file, given its open handle
-- TODO: rewrite to process multiple URLs at once
updateURLFile :: FilePath -> String -> IO ()
updateURLFile fp url = undefined
  where
    deleteFirstLine = undefined

-- read file containing list of words to ignore into Set structure
readIgnoreFile :: FilePath -> IO (Set String)
readIgnoreFile = fmap (Set.fromList . lines) . readFile

-- request a page
httpRequest :: URL -> IO String
httpRequest = simpleHTTP . getRequest >=> getResponseBody

sectionTag :: String -> [Tag String] -> [[Tag String]]
sectionTag tag = sections (~== TagOpen tag [])

-- get the title of a page
getTitle :: [Tag String] -> String
getTitle = unwords
         . map (fromTagText . head . filter isTagText)
         . sectionTag "title"

-- get the text content of a page
getBody :: [Tag String] -> [String]
getBody = harvest . sectionTag "p"
  where
    harvest  = map niceText . concatMap getText
    getText  = words . fromTagText . head . filter isTagText
    niceText = filter (\c -> isAlpha c || isSpace c)

-- get the list of all links on a page
getLinks :: [Tag String] -> [String]
getLinks = map getHref . sectionTag "a"
  where
    getHref = fromAttrib "href" . head . filter isTagOpen

-- type Page = (Title, Words, Links)
type Page = (String, [String], [String])

-- unified page get function
getPage :: String -> Page
getPage page =
    let tags = parseTags page
    in (getTitle tags, getBody tags, getLinks tags)

-----------------------------------------------------------------------------

-- for testing
testPage = httpRequest "http://stackoverflow.com/questions/1012573/getting-started-with-haskell"

testGetPage = fmap getPage testPage

-----------------------------------------------------------------------------

-- | Given a URL and the list of words on that page, produce a map from the
-- words to duples containing the URL and the frequency count.
makeWordFreqMap :: String     -- ^ URL of the page
                  -> [String]   -- ^ raw word content of the page
                  -> Map String (String, Int)
                                -- ^ a map of words to a (page, frequency) duple
makeWordFreqMap = loop Map.empty
  where
    loop freqMap _ [] = freqMap
    loop freqMap page (w:ws) = loop (wordEntry page w freqMap) page ws
    wordEntry page word freqMap =
        if Map.member word freqMap
            then Map.adjust incrEntry word freqMap
            else Map.insert word (page, 1) freqMap
    incrEntry (pg, cnt) = (pg, succ cnt)

-----------------------------------------------------------------------------

-- for testing:
-- should output the word frequencies for the test page
testWordFreq = do
    (title, ws, links) <- testGetPage
    print $ makeWordFreqMap title ws

-----------------------------------------------------------------------------

defaultCrawledFile :: String
defaultCrawledFile = "data/crawled.txt"

-- exception instance to handle possible exceptions from the crawler
data CrawlerException = RedisError
                      | ServerError
                      deriving (Show, Typeable)

instance Exception CrawlerException

-- crawler exception handler
crawlerHandler :: Handle -> CrawlerException -> IO ()
crawlerHandler urlsHandle exc = do
    let err = show (exc :: CrawlerException)
    hPutStr stderr $ "crawler exited with" ++ err
    hClose urlsHandle

-- take a URL of a page to crawl and a connection to Redis server;
-- crawl the page and stash the results in the server
crawlPage :: URL -> IO ()
crawlPage url = do
    result <- undefined
    e <- redisStore ("test", ["test"], ["test"])
    return ()

deleteLine :: Handle -> IO ()
deleteLine hdl = loop
  where
    loop = do
        char <- hLookAhead hdl
        hPutChar hdl '\0'
        unless (char == '\n') loop

runCrawler :: IO ()
runCrawler = do
    urlsHandle <- openFile defaultURLFile ReadWriteMode
    crawledHandle <- openFile defaultCrawledFile AppendMode
    forever $ do
        url <- hGetLine urlsHandle
        catch (crawlPage url) (crawlerHandler urlsHandle)
        deleteLine hdl
        hPutStrLn url crawledHandle

redisStore :: Page -> IO ()
redisStore (title, ws, links) = do
    con <- connect databaseInfo
    e <- runRedis con $ do
        echo . BC.pack $ "test"
    case e of
        Left l  -> do
            hPutStr stderr $ "redis replied with " ++ (show l)
            throwIO RedisError
        Right r -> return ()
