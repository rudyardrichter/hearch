module Main (main) where

import Crawler
import Search

import System.Environment

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
