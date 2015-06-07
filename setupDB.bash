#!/usr/bin/env bash

sqlite3 data/words.db "CREATE TABLE words (word TEXT PRIMARY KEY, page TEXT, freq INTEGER);"
