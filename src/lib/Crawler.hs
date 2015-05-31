{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module Crawler

{-
 - TODO: write module description
 -}

module Crawler where

import Control.Monad
import Control.Exception
import Data.Char
import Data.Typeable
import Database.Redis
import Network.HTTP hiding (Connection)
import System.IO
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as BC

import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------------------------------------

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

getTitle :: [Tag String] -> String
getTitle = unwords
         . map (fromTagText . head . filter isTagText)
         . sectionTag "title"

-- get the text content of a page;
-- acceptable content is extracted by validTags and harvested
getBody :: [Tag String] -> [String]
getBody = harvest . sectionTag "p"
  where
    harvest  = map niceText . concatMap getText
    getText  = words . fromTagText . head . filter isTagText
    niceText = filter (\c -> isAlpha c || isSpace c)

getLinks :: [Tag String] -> [String]
getLinks = map getHref . sectionTag "a"
  where
    getHref = fromAttrib "href" . head . filter isTagOpen

{-
 - Possibly better as:

data Page = Data String [String] [String]

 - ?
 -}

-- type Page = (Title, Words, Links)
type Page = (String, [String], [String])

getPage :: String -> Page
getPage page =
    let tags = parseTags page
    in (getTitle tags, getBody tags, getLinks tags)

----------------------------------------------------------------------

-- for debugging
testPage = httpRequest "http://stackoverflow.com/questions/1012573/getting-started-with-haskell"

testGetPage = fmap getPage testPage

----------------------------------------------------------------------

-- exception instance to handle possible exceptions from the crawler
data CrawlerException = RedisError
                      | ServerError
                      deriving (Show, Typeable)

instance Exception CrawlerException

-- crawler exception handler
crawlerHandler :: Handle -> CrawlerException -> IO ()
crawlerHandler urls exc = do
    let err = show (exc :: CrawlerException)
    hPutStr stderr $ "crawler exited with" ++ err
    hClose urls

-- take a URL of a page to crawl and a connection to Redis server;
-- crawl the page and stash the results in the server
crawlPage :: URL -> Connection -> IO ()
crawlPage url con = do
    result <- undefined
    e <- runRedis con $ sadd (BC.pack url) result
    case e of
        Left l  -> do
            hPutStr stderr $ "redis replied with " ++ (show l)
            throwIO RedisError
        Right r -> return ()
