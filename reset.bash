#!/usr/bin/env bash
# USE WITH CARE.

# make backups of the database files
cp data/words.db data/backup/words.db
cp data/urls.db data/backup/urls.db
cp data/crawled.db data/backup/crawled.db

# remove the existing database files
rm data/words.db
rm data/urls.db
rm data/crawled.db

# setup the database files again
sqlite3 data/words.db "CREATE TABLE words (word TEXT, page TEXT, freq INTEGER, views INTEGER);"
sqlite3 data/urls.db "CREATE TABLE urls (url TEXT);"
sqlite3 data/urls.db "INSERT INTO urls (url) VALUES ('/questions/1012573/getting-started-with-haskell');"
sqlite3 data/crawled.db "CREATE TABLE crawled (url TEXT);"
