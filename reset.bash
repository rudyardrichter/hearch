#!/usr/bin/env bash
# USE WITH CARE.

cp data/crawled.txt data/backup/crawled.txt
cp data/urls.txt data/backup/urls.txt
cp data/words.db data/backup/words.db

rm data/words.db
bash setupDB.bash

cp data/crawledDef.txt data/crawled.txt
cp data/urlsDef.txt data/urls.txt
