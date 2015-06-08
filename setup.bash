#!/usr/bin/env bash

tar -xzf data.tar.gz
sqlite3 data/words.db "CREATE TABLE words (word TEXT, page TEXT, freq INTEGER, views INTEGER);"

cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal build
