module Main (main) where

import Crawler
import Database
import Search

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

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " ++ progName ++ " --crawl | --search"
    exitWith $ ExitFailure 1
