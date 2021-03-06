#!/usr/bin/env bash

# unpack the data/ tarball
tar -xzf data.tar.gz

# setup the SQLite tables
sqlite3 data/words.db "CREATE TABLE words (word TEXT, page TEXT, freq INTEGER, views INTEGER);"
sqlite3 data/urls.db "CREATE TABLE urls (url TEXT);"
sqlite3 data/urls.db "INSERT INTO urls (url) VALUES ('/questions');"
sqlite3 data/crawled.db "CREATE TABLE crawled (url TEXT);"

# setup the test files
sqlite3 data/testWords.db "CREATE TABLE words (word TEXT, page TEXT, freq INTEGER, views INTEGER);"
sqlite3 data/testURLs.db "CREATE TABLE urls (url TEXT);"
sqlite3 data/testURLs.db "INSERT INTO urls (url) VALUES ('/questions/1012573/getting-started-with-haskell');"
sqlite3 data/testCrawled.db "CREATE TABLE crawled (url TEXT);"
sqlite3 data/testCrawled.db "INSERT INTO crawled (url) VALUES ('/test-url');"

# run cabal setup
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal build
