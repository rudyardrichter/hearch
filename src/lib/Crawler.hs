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
import Network.HTTP hiding (Connection)
import System.IO
import Text.HTML.TagSoup

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

-- This section contains functions for extracting data from webpages.

type URL = String

-- Default filepath for the list of URLs to be crawled.
defaultURLFile :: FilePath
defaultURLFile = "data/urls.txt"

-- Default filepath for the list of words to ignore.
defaultIgnoreFile :: FilePath
defaultIgnoreFile = "data/ignore.txt"

-- Read the ignore file into a Set structure.
readIgnoreFile :: FilePath -> IO (Set String)
readIgnoreFile = fmap (Set.fromList . lines) . readFile

-- Request the HTML content of a page.
httpRequest :: URL -> IO String
httpRequest = simpleHTTP . getRequest >=> getResponseBody

-- Extract the sections beginning with a given tag from a tag structure.
sectionTag :: String          -- ^ the tag to look for
           -> [Tag String]    -- ^ the parsed tagsoup
           -> [[Tag String]]  -- ^ list of tag sections beginning with the
                              -- desired tag
sectionTag tag = sections (~== TagOpen tag [])

-- Get the title of a page.
getTitle :: [Tag String] -> String
getTitle = unwords
         . map (fromTagText . head . filter isTagText)
         . sectionTag "title"

-- Get the text content of a page.
getBody :: [Tag String] -> [String]
getBody = harvest . sectionTag "p"
  where
    harvest  = map niceText . concatMap getText
    getText  = words . fromTagText . head . filter isTagText
    niceText = filter (\c -> isAlpha c || isSpace c)

-- Get the list of all links on a page.
getLinks :: [Tag String] -> [String]
getLinks = map getHref . sectionTag "a"
  where
    getHref = fromAttrib "href" . head . filter isTagOpen

-----------------------------------------------------------------------------

-- This section contains functions for manipulating data from webpages, and
-- ultimately generating a Page result from a URL.

-- type Page = (Title, Words, Links)
type Page = (String, [String], [String])

-- unified page get function
getPage :: URL      -- ^ the URL of the page to retrieve
        -> IO Page  -- ^ the Page result
getPage url = do
    tags <- httpRequest url >>= return . parseTags
    return (getTitle tags, getBody tags, getLinks tags)


-- for testing
testPage :: String
testPage = "http://stackoverflow.com/questions/1012573/getting-started-with-haskell"

testGetPage :: IO Page
testGetPage = getPage testPage

-- | Given a URL and the list of words on that page, produce a map from the
-- words to duples containing the URL and the frequency count.
makeWordFreqMap :: String     -- ^ URL of the page
                -> [String]   -- ^ raw word content of the page
                              -- (along with weighted title---see crawlPage)
                -> Map String (String, Int)
                              -- ^ map of words to a (page, frequency) duple
makeWordFreqMap = loop Map.empty
  where
    loop freqMap _ [] = freqMap
    loop freqMap page (w:ws) = loop (wordEntry page w freqMap) page ws
    wordEntry page word freqMap =
        if Map.member word freqMap
            then Map.adjust incrEntry word freqMap
            else Map.insert word (page, 1) freqMap
    incrEntry (pg, cnt) = (pg, succ cnt)

-- for testing:
-- should output the word frequencies for the test page
testWordFreq :: IO ()
testWordFreq = do
    (title, ws, _) <- testGetPage
    print $ makeWordFreqMap title ws

-----------------------------------------------------------------------------

-- This section contains functions for running the crawler.

-- Default filepath for the file of URLs which have been crawled already.
defaultCrawledFile :: String
defaultCrawledFile = "data/crawled.txt"

-- exception instance to handle possible exceptions from the crawler
data CrawlerException = RedisError
                      | ServerError
                      deriving (Show, Typeable)

instance Exception CrawlerException

-- | Exception handler for exiting from the crawler. Takes the handles from
-- urls.txt and crawled.txt to make sure they are closed.
crawlerHandler :: Handle           -- ^ handle for urls.txt
               -> Handle           -- ^ handle for crawled.txt
               -> CrawlerException -- ^ the exception to catch
               -> IO [String]      -- ^ empty [String], to match return type
                                   -- of crawlPage
crawlerHandler urlsHandle crawledHandle exc = do
    let err = show (exc :: CrawlerException)
    hPutStr stderr $ "crawler exited with" ++ err
    hClose urlsHandle
    hClose crawledHandle
    return [""]

-- | Crawl a page and stash the results in the server.
crawlPage :: URL          -- ^ the URL of a page to crawl
          -> Set String   -- ^ a set of words to ignore
          -> IO [String]  -- ^ hyperlinks found on that page
crawlPage url ignoreWords = do
    -- obtain page result
    (title, ws, links) <- getPage url
    -- (note: Stack Overflow-specific)
    -- discard "Stack Overflow" from title text
    let titleWords = takeWhile (/= "Stack") $ words title
    -- put the title words in 3 times to give them more weight
    let weightedWords = filter (flip Set.notMember ignoreWords)
                      $ (concat $ replicate 3 titleWords) ++ ws
    -- store page into word/page-frequency map
    let freqMap = makeWordFreqMap url weightedWords
    -- call storeFreqMap from Database to store the result
    storeFreqMap freqMap
    return links

-- | The main routine for the crawler, exported to Main. Is essentially a
-- wrapper of runCrawlPage.
runCrawler :: Int -> IO ()
runCrawler n = do
    -- load the list of ignored words and open the crawled links file
    ignoreWordsSet <- readIgnoreFile defaultIgnoreFile
    crawledHandle <- openFile defaultCrawledFile AppendMode
    if n > 0
        then doNTimes n $ runCrawlPage ignoreWordsSet crawledHandle
        else forever $ runCrawlPage ignoreWordsSet crawledHandle
  where
    doNTimes n action = action >> doNTimes (pred n) action

-- Subroutine of runCrawer. Performs crawling on one page.
-- (Having the option to crawl one page at a time also facilitates
-- testing/debugging.)
runCrawlPage :: Set String -> Handle -> IO ()
runCrawlPage ignoreWordsSet crawledHandle = do
    -- open the URL file and get a new URL to crawl
    urlsHandle <- openFile defaultURLFile ReadWriteMode
    url <- hGetLine urlsHandle
    -- crawl the page and collect the links from the page
    allLinks <- catch (crawlPage url ignoreWordsSet)
                      (crawlerHandler urlsHandle crawledHandle)
    -- filter for links which go to other questions,
    -- and format the links so they can be used
    let newLinks = formatLinks . filter correctDomain $ allLinks
    -- remove the URL we just crawled from the URL file,
    -- and write it to the file of links which have been crawled
    deleteLine urlsHandle
    hPutStrLn crawledHandle url
    -- close the URL file handle
    hClose urlsHandle
    -- append all the new URLs to the URL file
    mapM_ (appendFile defaultURLFile) newLinks

-- Helper function for runCrawlPage. Deletes the first line of an open file.
deleteLine :: Handle -> IO ()
deleteLine hdl = loop
  where
    loop = do
        char <- hLookAhead hdl
        hPutChar hdl '\0'
        unless (char == '\n') loop

-- Helper function for runCrawler. Checks if a URL goes to the desired
-- domain (stackoverflow.com/questions). These URLs will all be linked from
-- the current page as beginning with "/q" or "/questions".
correctDomain :: String -> Bool
correctDomain = beginsWith "/q"
  where
    beginsWith [] _ = True
    beginsWith _ [] = False
    beginsWith (x:xs) (y:ys) = x == y && beginsWith xs ys

-- Helper function for runCrawlPage. Takes the links which were filtered with
-- correctDomain and formats them as usable URLs (prepending
-- "http://stackoverflow.com").
formatLinks :: [String] -> [String]
formatLinks = map ("http://stackoverflow.com" ++)
