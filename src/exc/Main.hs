module Main (main) where

import Crawler
import Search

import Control.Exception
import Control.Monad (when)
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) usage
    let action = head args
    let arg = head $ tail args
    e <- try (readIO arg) :: IO (Either IOError Int)
    case e of
        Left _  -> usage
        Right r -> run action r

run :: String -> Int -> IO ()
run action n = case action of
    "--crawl"  -> runCrawler n
    "--search" -> runSearch n
    _          -> usage

usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $
        "usage: " ++ progName ++ " --crawl <n pages> | --search <n results>"
    exitFailure
