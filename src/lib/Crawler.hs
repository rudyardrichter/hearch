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

module Crawler where

import Database

import Control.Exception
import Control.Monad
import Data.Char
import Network.HTTP hiding (Connection)
import System.Exit
import System.IO
import System.Posix.Signals
import System.Process
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

-- Read a file into a Set structure.
readFileToSet :: FilePath -> IO (Set String)
readFileToSet = fmap (Set.fromList . lines) . readFile

-- Request the HTML content of a page.
httpRequest :: URL -> IO String
httpRequest = simpleHTTP . getRequest >=> getResponseBody

-- Extract the sections beginning with a given tag from a tag structure.
sectionTag :: String             -- ^ the tag to look for
           -> [(String, String)] -- ^ attributes of the tag (as in tagsoup)
           -> [Tag String]       -- ^ the parsed tagsoup
           -> [[Tag String]]     -- ^ list of tag sections beginning with the
                                 -- desired tag
sectionTag tag attr = sections (~== TagOpen tag attr)

-- Get the title of a page.
getTitle :: [Tag String] -> String
getTitle = unwords
         . map (fromTagText . head . filter isTagText)
         . sectionTag "title" []

-- Get the text content of a page.
getBody :: [Tag String] -> [String]
getBody = harvest . sectionTag "p" []
  where
    harvest  = map niceText . concatMap getText
    getText  = words . fromTagText . head . filter isTagText
    niceText = filter (\c -> isAlpha c || isSpace c)

-- Get the list of all links on a page.
getLinks :: [Tag String] -> [String]
getLinks = map getHref . sectionTag "a" []
  where
    getHref = fromAttrib "href" . head . filter isTagOpen

-- Extract the number of views from a normally-formatted Stack Overflow page.
-- Works only on this specific format; returns 1 otherwise.
getViews :: [Tag String] -> IO Int
getViews tags = do
    let views = retrieveViews tags
    e <- try (readIO views) :: IO (Either IOError Int)
    case e of
        Left _  -> return 1
        Right r -> return r
  where
    retrieveViews = headDef "1" . words . ix3Def "1"
                  . map getText
                  . sectionTag "p" [("class", "label-key")]
    getText = fromTagText . headDef tagDef . tailDef tagDef . filter isTagText
    -- (highly situational) functions for exceptionless retrieval from lists.
    -- basically, we just want the views if possible, or else just return 1.
    -- "Index 3 with Default"
    ix3Def _ (_:_:_:x:_) = x
    ix3Def y _ = y
    tailDef y [] = [y]
    tailDef _ xs = tail xs
    headDef y [] = y
    headDef _ xs = head xs
    tagDef = TagText "1"

-----------------------------------------------------------------------------

-- This section contains functions for manipulating data from webpages, and
-- ultimately generating a Page result from a URL.

-- type Page = (Views, Title, Words, Links)
type Page = (Int, String, [String], [String])

-- unified page get function
getPage :: URL      -- ^ the URL of the page to retrieve
        -> IO Page  -- ^ the Page result
getPage url = do
    tags <- httpRequest url >>= return . parseTags
    views <- getViews tags
    return (views, getTitle tags, getBody tags, getLinks tags)


-- for testing
testPage :: String
testPage = "http://stackoverflow.com/questions/1012573/getting-started-with-haskell"

testGetPage :: IO Page
testGetPage = getPage testPage

-- | Given a URL and the list of words on that page, produce a map from the
-- words to duples containing the URL and the frequency count.
makeWordFreqMap :: Int       -- ^ the number of views
                -> String    -- ^ URL of the page
                -> [String]  -- ^ raw word content of the page
                             -- (along with weighted title---see crawlPage)
                -> Map String (String, Int, Int)
                             -- ^ map words to (page, frequency, views)
makeWordFreqMap = loop Map.empty
  where
    loop freqMap _ _ [] = freqMap
    loop freqMap views page (w:ws) =
        loop (wordEntry views page w freqMap) views page ws
    wordEntry views page word freqMap =
        if Map.member word freqMap
            then Map.adjust incrEntry word freqMap
            else Map.insert (map toLower word) (page, 1, views) freqMap
    incrEntry (pg, cnt, views) = (pg, succ cnt, views)

-- for testing:
-- should output the word frequencies for the test page
testWordFreq :: IO ()
testWordFreq = do
    (views, title, ws, _) <- testGetPage
    print $ makeWordFreqMap views title ws

-----------------------------------------------------------------------------

-- This section contains functions for running the crawler.

-- Default filepath for the file of URLs which have been crawled already.
defaultCrawledFile :: String
defaultCrawledFile = "data/crawled.txt"

-- | Crawl a page and stash the results in the server. The innermost
-- crawling function (inside runCrawler and runCrawlPage): manages data
-- extraction from a single page, retrieving the title, the word content,
-- the hyperlinks, and the number of views, which it stores in the SQLite
-- table. It returns the list of links it retrieved.
crawlPage :: URL          -- ^ the *unformatted* URL of a page to crawl
          -> Set String   -- ^ a set of words to ignore
          -> IO [String]  -- ^ hyperlinks found on that page
crawlPage url ignoreWords = do
    -- obtain page result
    (views, title, ws, links) <- getPage $ formatLink url
    -- (note: Stack Overflow-specific)
    -- discard "Stack Overflow" from title text
    let titleWords = takeWhile (/= "Stack") $ words title
    -- put the title words in 3 times to give them more weight
    let weightedWords = filter (flip Set.notMember ignoreWords)
                      $ (concat $ replicate 3 titleWords) ++ ws
    -- store page into word/page-frequency map
    let freqMap = makeWordFreqMap views url weightedWords
    -- call storeFreqMap from Database to store the result
    storeFreqMap freqMap
    return links

-- | The main routine for the crawler, exported to Main. Is essentially a
-- wrapper of runCrawlPage, feeding it the default set of ignore words and
-- the initial crawledSet. The outermost crawling function.
runCrawler :: Int -> IO ()
runCrawler n = do
    -- load the list of ignored words and open the crawled links file
    ignoreWordsSet <- readFileToSet defaultIgnoreFile
    crawledSet <- readFileToSet defaultCrawledFile
    putStr "Running the crawler"
    hFlush stdout
    if n > 0
        then void $
             foldr (<=<) return (replicate n (runCrawlPage ignoreWordsSet))
                 $ crawledSet
        else forever $ runCrawlPage ignoreWordsSet crawledSet

-- | Performs crawling on one page. The intermediary crawling function (is
-- called by runCrawler, runs crawlPage): manages the file of URLs to crawl,
-- appends crawled URLs to the crawled file and also adds them to the
-- crawledSet, which it returns to the next call of runCrawlPage (see above,
-- in runCrawler).
runCrawlPage :: Set String -> Set String -> IO (Set String)
runCrawlPage ignoreWordsSet crawledSet = do
    putStr "."
    hFlush stdout
    -- ! block SIGINT until we are done crawling the page
    blockSignals $ addSignal sigINT emptySignalSet
    -- get a new URL to crawl
    url <- withFile defaultURLFile ReadMode hGetLine
    -- crawl the page and collect the links from the page
    allLinks <- crawlPage url ignoreWordsSet
    -- let /q and /questions go through
    let redundant url = if length url > 10
        then not . beginsWith url
        else \_ -> True
    -- filter for links which go to other questions, filter out links which
    -- have already been crawled, then format the links so they can be used
    let newLinks = filter (redundant url)
                 . filter (flip Set.notMember crawledSet)
                 . filter correctDomain
                 $ allLinks
    -------------
    -- CLEANUP
    -------------
    -- runCrawlPage should fail safely enough if crawlPage (above) raised an
    -- exception; we simply won't end up removing that URL from the URL file
    -- or adding the new links to the file for crawling, so the crawler can
    -- safely resume its operations the next time it is run.
    -------------
    -- remove the URL we just crawled from the URL file, close the URL file
    -- handle, and append all the new URLs to the URL file via appendFile
    -- ! literal system command to remove first line of urls.txt
    -- ! urls.txt must be closed during this operation
    processHandle <- runCommand "tail -n +2 data/urls.txt > data/urls.txt"
    exitcode <- waitForProcess processHandle
    withFile defaultURLFile AppendMode $ \hdl ->
        mapM_ (hPutStrLn hdl) newLinks
    -- add the URL we just crawled to the crawled file
    withFile defaultCrawledFile AppendMode $ \hdl ->
        hPutStrLn hdl url
    -- ! check for SIGINT
    unblockSignals fullSignalSet
    return $ Set.insert url crawledSet

-- Helper function for runCrawler. Checks if a URL goes to the desired
-- domain (stackoverflow.com/questions). These URLs will all be linked from
-- the current page as beginning with "/q" or "/questions".
correctDomain :: String -> Bool
correctDomain url = beginsWith "/q" url && notDisallowed url
  where
    notDisallowed url = not . any (flip beginsWith url) $ disallowed
    -- from stackoverflow.com/robots.txt
    disallowed = ["/questions/*answertab=*"
                 ,"/questions/tagged/***"
                 ,"/questions/tagged/*%20*"
                 ,"/questions/*/answer/submit"]

beginsWith :: (Eq a) => [a] -> [a] -> Bool
beginsWith [] _ = True
beginsWith _ [] = False
beginsWith (x:xs) (y:ys) = x == y && beginsWith xs ys

-- Helper function for runCrawlPage. Takes the links which were filtered with
-- correctDomain and formats them as usable URLs (prepending
-- "http://stackoverflow.com").
formatLinks :: [String] -> [String]
formatLinks = map formatLink

formatLink :: String -> String
formatLink = ("http://stackoverflow.com" ++)
