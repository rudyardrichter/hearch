A primitive search engine written in Haskell.

## Modules

#### Crawler.hs

The web crawler.

#### Database.hs

Manages the Redis database (using `hedis`).

#### Search.hs

Will contain searching and ranking algorithms.

## Data

#### Database

TODO

#### URL File

`data/urls.txt` stores a list of URLs to crawl.
As the crawler finds new hyperlinks in a page,
it appends them to the file.
When it begins processing a page,
it removes its URL from the file.

#### Ignore File

`data/ignore.txt` stores a list of words to ignore
when counting word frequency for a page.
