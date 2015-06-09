#!/usr/bin/env bash

# remove the current test files
rm -f data/testWords.db
rm -f data/testURLs.db
rm -f data/testCrawled.db

# set up the testing files
sqlite3 data/testWords.db "CREATE TABLE words (word TEXT, page TEXT, freq INTEGER, views INTEGER);"
sqlite3 data/testURLs.db "CREATE TABLE urls (url TEXT);"
sqlite3 data/testURLs.db "INSERT INTO urls (url) VALUES ('/questions/1012573/getting-started-with-haskell');"
sqlite3 data/testCrawled.db "CREATE TABLE crawled (url TEXT);"
sqlite3 data/testCrawled.db "INSERT INTO crawled (url) VALUES ('/test-url');"

cabal test --log=/dev/stdout --test-options="--color"
