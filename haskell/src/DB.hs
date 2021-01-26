module DB (conn) where

import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, connect, defaultConnectInfo)

conn :: IO Connection
conn =
  connect
    defaultConnectInfo
      { connectHost = "localhost",
        connectDatabase = "brute",
        connectUser = "ksb",
        connectPassword = ""
      }
