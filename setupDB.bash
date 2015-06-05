#!/usr/bin/env bash

sqlite3 words.db "CREATE TABLE words (word TEXT PRIMARY KEY, page TEXT, freq INTEGER);"
