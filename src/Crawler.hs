-- module Crawler

{-
 - TODO: write module description
 -}

module Crawler where

import Text.HTML.TagSoup

import Data.Set (Set)
import qualified Data.Set as Set

type URL = String

-- read file containing list of seed URLs
readURLFile :: FilePath -> IO [URL]
readURLFile = fmap lines . readFile

-- read file containing list of words to ignore into Set structure
readIgnoreFile :: FilePath -> IO (Set String)
readIgnoreFile = fmap (fromList . lines) . readFile

-- request a page
httpRequest :: URL -> IO String
httpRequest = simpleHTTP . getRequest >=> getResponseBody
