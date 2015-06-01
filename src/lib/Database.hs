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

databaseInfo :: ConnectInfo
databaseInfo = defaultConnectInfo
