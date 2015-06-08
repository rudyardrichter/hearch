A primitive search engine written in Haskell.

TODO
  * ~~finish `freqSort` algorithm in Search~~
  * ~~finish writing `storeFreqMap`~~
  * ~~limit crawling to single domain~~
  * ~~make sure crawler never visits same page twice~~
  * write test suite
  * setup: make empty `crawled.txt`, make `urls.txt` with seed URL
  * complete haddock documentation
  * ~~implement word ignore in crawler's processing functions~~
  * ~~make ranking slightly less naive~~

Maybe do
  * make ranking even less naive
  * implement an `AND` operator for searches

## Usage

#### Setup

Run `setupDB.bash` once before doing anything to create the SQLite database.

    $ cabal install --only-dependencies --enable tests
    $ cabal configure --enable-tests
    $ cabal build

    $ cabal run

    $ cabal test

I have suppressed warnings from name shadowing and unused bindings. Note that
`cabal test` will not print out its results if all the tests are passing, so
instead run with one of the following:

    $ cabal test --log=/dev/stdout
    $ bash runTests.bash

and `cabal` will print out the results of all the tests. `runTests.bash`
also colorizes the `cabal test` output for 15% more fun.

#### Searching

Searches should be entered in lowercase.

## Modules

#### Crawler.hs

The web crawler. It restricts itself to browsing
[stackoverflow.com/questions](http://stackoverflow.com/questions), and the
seed URL is
[http://stackoverflow.com/questions/1012573/getting-started-with-haskell](http://stackoverflow.com/questions/1012573/getting-started-with-haskell).
Applicable URLs from
[stackoverflow.com/robots.txt](http://stackoverflow.com/robots.txt) are
hard-coded into the crawler.

#### Database.hs

~~Manages the Redis database (using `hedis`).~~

Functions for storing and retrieving word/page-frequency-views entries from the
SQL database.

#### Search.hs

Exports `runSearch`, which is the IO loop in which the user may perform
searches. A search consisting of multiple words is assumed to mean

    word1 OR word2 OR ...

#### Rank.hs

Will contain ranking algorithms.

## Data

#### Database

Hearch uses SQLite for storing word/page-frequency-views entries generated by the
crawler. The columns of the table are

    word  |  page  |  frequency  |  views

where `word` occurred `frequency` times on `page`, and `page` has `views`
views.


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
