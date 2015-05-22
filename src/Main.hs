{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Crawler
import Database

import Control.Exception
import Control.Monad
import Database.Redis
import Data.Typeable
import System.IO

main :: IO ()
main = return ()

runCrawler :: IO ()
runCrawler = do
    urls <- openFile defaultURLFile ReadWriteMode
    con <- connect databaseInfo
    -- TODO: delete line from file when read
    forever $ do
        url <- hGetLine urls
        catch (crawlPage url con) (crawlerHandler urls)

-- TODO
data CrawlerException = CrawlerException deriving (Show, Typeable)
instance Exception CrawlerException

crawlerHandler :: Handle -> CrawlerException -> IO ()
crawlerHandler urls exc = do
    let err = show (exc :: CrawlerException)
    hPutStr stderr $ "crawler exited with" ++ err
    hClose urls

runSearch :: IO ()
runSearch = undefined
