module Models (insertOne) where

import Database.HDBC.ODBC (connectODBC, Connection)
import Database.HDBC as HDBC

getDatabaseName :: String
getDatabaseName = "DSN=MariaDBTest"

getTableName :: String
getTableName = "results"

getDatabaseConnection :: IO Connection
getDatabaseConnection = connectODBC getDatabaseName

-- data Player = Player {
--   ID :: Int,
--   Username :: String,
--   Score :: Int,
--   Rank :: Int
-- }

insertOne :: IO ()
insertOne = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ "INSERT INTO " ++ getTableName ++ " VALUES (?, ?, ?, ?)"
  execute stmt [toSql (1 :: Int), toSql "Role", toSql (10 :: Int), toSql (1 :: Int)]
  commit conn
  disconnect conn
