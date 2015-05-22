{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module Crawler

{-
 - TODO: write module description
 -}

module Crawler where

import Control.Monad
import Control.Exception
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
updateURLFile :: Handle -> String -> IO ()
updateURLFile h url = do
    hSeek h SeekFromEnd 0
    hPutStr h url
    hSeek h AbsoluteSeek 0

-- read file containing list of words to ignore into Set structure
readIgnoreFile :: FilePath -> IO (Set String)
readIgnoreFile = fmap (Set.fromList . lines) . readFile

-- request a page
httpRequest :: URL -> IO String
httpRequest = simpleHTTP . getRequest >=> getResponseBody

-- get the text content of a page (which is a single string);
-- acceptable content is extracted by validTags,
-- and filtered by validText
getWords :: String -> [String]
getWords = filter validText . map innerText . validTags . parseTags
  where
    validTags = sections (== TagOpen "p" [])
    -- TODO
    validText = \_ -> True

----------------------------------------------------------------------

-- exception instance to handle possible exceptions from the crawler
data CrawlerException = RedisError
                      | ServerError
                      deriving (Show, Typeable)

instance Exception CrawlerException

crawlerHandler :: Handle -> CrawlerException -> IO ()
crawlerHandler urls exc = do
    let err = show (exc :: CrawlerException)
    hPutStr stderr $ "crawler exited with" ++ err
    hClose urls

crawlPage :: URL -> Connection -> IO ()
crawlPage url con = do
    result <- undefined
    e <- runRedis con $ sadd (BC.pack url) result
    case e of
        Left _  -> throwIO RedisError
        Right r -> undefined
    return ()
