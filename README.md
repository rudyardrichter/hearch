A primitive search engine written in Haskell.

TODO
  * limit crawling to single domain
  * complete haddock documentation
  * ~~figure out how to package data into redis~~
  * implement word ignore in crawler's processing functions

## Usage

Run `setupDB.bash` once before doing anything to create the SQLite database.

    $ cabal install --only-dependencies --enable tests
    $ cabal configure --enable-tests
    $ cabal build

    $ cabal run

    $ cabal test

## Modules

#### Crawler.hs

The web crawler.

#### Database.hs

~~Manages the Redis database (using `hedis`).~~

Functions for storing and retrieving word/page-frequency entries from the
SQL database.

#### Search.hs

Will contain search algorithms.

#### Rank.hs

Will contain ranking algorithms.

## Data

#### Database

Hearch uses SQLite for storing word/page-frequency entries generated by the
crawler.

#### URL File

`data/urls.txt` stores a list of URLs to crawl. As the crawler finds new
hyperlinks in a page, it appends them to the file. When it begins processing
a page, it removes that page's URL from the file.

#### Crawled File

`data/crawled.txt` stores a list of URLs which have already been crawled.
The crawler appends URLs to this file immediately after it finishes
processessing them.

#### Ignore File

`data/ignore.txt` stores a list of words to ignore when counting word
frequency for a page.
