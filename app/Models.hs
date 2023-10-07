module Models (Player(..), insertOne, insertMany) where

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

convertPlayerFieldValuesToSqlArray :: Player -> [SqlValue]
convertPlayerFieldValuesToSqlArray player = [toSql $ id_num player, toSql $ username player, toSql $ score player, toSql $ rank player]

insertOne :: Player ->  IO ()
insertOne player = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ "INSERT INTO " ++ getTableName ++ " VALUES (?, ?, ?, ?)"
  execute stmt $ convertPlayerFieldValuesToSqlArray player
  commit conn
  disconnect conn

insertMany :: [Player] -> IO ()
insertMany players = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ "INSERT INTO " ++ getTableName ++ " VALUES (?, ?, ?, ?)"
  let playerSqlArrays = map convertPlayerFieldValuesToSqlArray players
  executeMany stmt playerSqlArrays
  commit conn
  disconnect conn
