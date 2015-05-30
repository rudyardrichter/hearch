module Main (main) where

import Crawler
import Database

import Control.Exception
import Control.Monad
import Database.Redis
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    args <- getArgs
    let flag = head args
    case flag of
        "--crawl"  -> runCrawler
        "--search" -> runSearch
        _          -> usage

runCrawler :: IO ()
runCrawler = do
    urls <- openFile defaultURLFile ReadWriteMode
    con <- connect databaseInfo
    -- TODO: delete line from file when read
    forever $ do
        url <- hGetLine urls
        catch (crawlPage url con) (crawlerHandler urls)

runSearch :: IO ()
runSearch = undefined

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " ++ progName ++ " --crawl | --search"
    exitWith $ ExitFailure 1
