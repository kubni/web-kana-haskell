module Models (Player(..), insertOne, insertMany, updateOne, updateMany, deleteOne, deleteMany, calculateNumberOfPages) where

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

-- TODO: There is a lot of duplicate code here...
-- It receives an already updated Player instance which it then just needs to use to update the actual database row.
-- The update on the haskell data type instance will happen in the controller via Lens probably

updateOne :: Player -> IO ()
updateOne updatedPlayer = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ " UPDATE " ++ getTableName ++ "\n \
                         \ SET score=?, rank=?\n \
                         \ WHERE id=?"
  execute stmt [toSql $ score updatedPlayer, toSql $ rank updatedPlayer, toSql $ id_num updatedPlayer]
  commit conn
  disconnect conn

updateMany :: [Player] -> IO ()
updateMany updatedPlayers = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ "  UPDATE " ++ getTableName ++ "\n \
                          \ SET score=?, rank=?\n \
                          \ WHERE id=?"
  let placeholderValues = map (\updatedPlayer -> [toSql $ score updatedPlayer, toSql $ rank updatedPlayer, toSql $ id_num updatedPlayer])  updatedPlayers
  executeMany stmt placeholderValues
  commit conn
  disconnect conn


-- TODO: id_name is enough for deletion, but for the sake of it having the same form as other db operations i have used Player -> IO() instead of Int -> IO() -- *subject to change*
deleteOne :: Player -> IO()
deleteOne player = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ " DELETE FROM " ++ getTableName ++ "\n \
                         \ WHERE id=?"
  execute stmt [toSql $ id_num player]
  commit conn
  disconnect conn

deleteMany :: [Player] -> IO()
deleteMany players = do
  conn <- getDatabaseConnection
  stmt <- prepare conn $ " DELETE FROM " ++ getTableName ++ "\n \
                         \ WHERE id=?"
  let placeholderValues = map (\player -> [toSql $ id_num player]) players
  executeMany stmt placeholderValues
  commit conn
  disconnect conn


-- Pagination logic
calculateNumberOfPages :: Int -> IO Int
calculateNumberOfPages playersPerPage = do
  conn <- getDatabaseConnection
  queryResult <- quickQuery' conn ("SELECT count(*) from " ++ getTableName) []   -- The result here will be something like [[x]] where x is the SqlValue number of items/rows in the table
  let totalNumberOfPlayers = fromSql $ head $ head queryResult :: Int
  let floatDivResult = (fromIntegral totalNumberOfPlayers :: Float) / (fromIntegral playersPerPage :: Float)
  return (ceiling floatDivResult :: Int)
