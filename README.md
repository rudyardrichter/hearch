A primitive search engine written in Haskell.

Known bugs
  * Crashes with `Exception: Prelude.head: empty list` when it finishes
    crawling every question on Stack Overflow

TODO
  * ~~write test suite~~
  * ~~rework `.txt` dependencies in crawler to database usage~~
  * ~~update tarball~~
  * ~~Instead of `urls.txt` and `crawled.txt`, make two more SQLite tables
    to accommodate the rest of the data~~
  * ~~fix getViews~~
  * ~~finish `freqSort` algorithm in Search~~
  * ~~finish writing `storeFreqMap`~~
  * ~~limit crawling to single domain~~
  * ~~make sure crawler never visits same page twice~~
  * ~~setup: extract `data.tar.gz` during setup~~
  * ~~implement word ignore in crawler's processing functions~~
  * ~~make ranking slightly less naive~~

The future
  * write a different version of the crawler which crawls locally around a
    chosen URL (as opposed to randomly, as it is now)
  * complete haddock documentation
  * make ranking even less naive
  * implement an `AND` operator for searches
  * allow the user to open URLs from the search function

## Usage

#### Setup & Compiling

Run

    $ bash setup.bash

and `setup.bash` will perform the necessary installation steps. This includes
cabal installation/configuration, so if Hearch is being installed into a
sandbox then that should be done prior to running `setup.bash`. This script
should be run only once per installation. Warnings from name shadowing and
unused bindings are suppressed.

#### Compiling & Testing

NOTE that `cabal test` will not run correctly, because the testing module
requires that the test files in `data` be set up correctly. Instead, testing
MUST be run by

    $ bash runTests.bash

which also colorizes the `cabal test` output for 15% more fun.

#### Crawling

To run the crawler:

    $ hearch --crawl n

where `n` is the number of pages to be crawled. Enter a negative number to
have Hearch crawl continuously until interrupted.

#### Searching

To run searching:

    $ hearch --search n

where `n` is the number of results to be displayed per search. Searches
should be entered in lowercase.

## Modules

#### Crawler.hs

The web crawler. It restricts itself to browsing
[stackoverflow.com/questions](http://stackoverflow.com/questions), which is
also the seed URL which the crawler begins from. Applicable URLs from
[stackoverflow.com/robots.txt](http://stackoverflow.com/robots.txt) are
hard-coded into the crawler.

#### Database.hs

Functions for storing and retrieving word/page-frequency-views entries from the
SQL database.

#### Search.hs

Exports `runSearch`, which is the IO loop in which the user may perform
searches. A search consisting of multiple words is assumed to mean

    word1 OR word2 OR ...

though the weight given to each search term is roughly normalized.

## Data

Data-related files are contained in the `data` folder. There is also a
sub-folder `data/backup` which stores all the files which were most recently
overwritten.

#### words.db

Hearch uses SQLite for storing word/page-frequency-views entries generated by the
crawler. The columns of the table are

    word  |  page  |  frequency  |  views

where `word` occurred `frequency` times on `page`, and `page` has `views`
views. The table is stored in `data/words.db`.

#### URL File

`data/urls.db` stores a single-column table of URLs to crawl. As the crawler
finds new hyperlinks in a page, it inserts them into the table. When it begins
processing a page, it removes that page's URL from the file.

#### Crawled File

`data/crawled.db` stores a single-column table of URLs which have already
been crawled. The crawler adds URLs to the table as it finishes processing
them.

#### Ignore File

`data/ignore.txt` stores a list of words to ignore when counting word
frequency for a page. The crawler loads these words into a Set structure
when it starts running.
