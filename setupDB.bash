#!/usr/bin/env bash

sqlite3 data/words.db "CREATE TABLE words (word TEXT, page TEXT, freq INTEGER, views INTEGER);"
