module Models (insertOne) where

import Database.HDBC.ODBC (connectODBC, Connection)
import Database.HDBC as HDBC

getDatabaseName :: String
getDatabaseName = "DSN=MariaDBTest"

getTableName :: String
getTableName = "results"

getDatabaseConnection :: IO Connection
getDatabaseConnection = connectODBC getDatabaseName

data Player = Player {
  id_num :: Int,
  username :: String,
  score :: Int,
  rank :: Int
 }

insertOne :: Player ->  IO ()
insertOne player = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ "INSERT INTO " ++ getTableName ++ " VALUES (?, ?, ?, ?)"
  execute stmt [toSql $ id_num player, toSql $ username player, toSql $ score player, toSql $ rank player]
  commit conn
  disconnect conn
