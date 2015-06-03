A primitive search engine written in Haskell.

TODO
    * complete haddock documentation
    * figure out how to package data into redis

## Modules

#### Crawler.hs

The web crawler.

#### Database.hs

Manages the Redis database (using `hedis`).

#### Search.hs

Will contain search algorithms.

#### Rank.hs

Will contain ranking algorithms.

## Data

#### Database

TODO: how to store Haskell data types to Redis server

#### URL File

`data/urls.txt` stores a list of URLs to crawl. As the crawler finds new
hyperlinks in a page, it appends them to the file. When it begins processing
a page, it removes its URL from the file.

#### Crawled File

`data/crawled.txt` stores a list of URLs which have already been crawled.

#### Ignore File

`data/ignore.txt` stores a list of words to ignore when counting word
frequency for a page.
