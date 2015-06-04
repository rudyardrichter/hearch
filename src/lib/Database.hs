-----------------------------------------------------------------------------
---- |
---- Module      :  Database
---- Copyright   :  (c) Rudyard Richter 2015
---- License     :  MIT
----
---- Maintainer  :  rudyardrichter@uchicago.edu
---- Stability   :  development
---- Portability :  non-portable
----
---- Provides functions to use for interfacing with the Redis database
---- (via hedis, the Haskell Redis client).
----
-----------------------------------------------------------------------------

module Database where

import Database.Redis

import qualified Data.ByteString.Char8 as B

databaseInfo :: ConnectInfo
databaseInfo = defaultConnectInfo

redisStore :: (String, [String], [String]) -> IO (Either Reply B.ByteString)
redisStore (title, ws, links) = do
    con <- connect databaseInfo
    e <- runRedis con $ do
        echo . B.pack $ "test"
    return e
