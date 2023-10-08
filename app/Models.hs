module Models (
  Player(..),
  insertOne, insertMany,
  updateOne, updateMany,
  deleteOne, deleteMany,
  calculateNumberOfPages, getScoreboardPage
  ) where

import Control.Exception (bracket)
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
 } deriving (Show)

withDBConnection = bracket getDatabaseConnection disconnect
commitWithDBConnection f = withDBConnection (\conn -> f conn >> commit conn)

insertOne :: Player -> IO ()
insertOne player = commitWithDBConnection
  (\conn -> do
      stmt <- prepare conn $ "INSERT INTO " ++ getTableName ++ " VALUES (?, ?, ?, ?)"
      execute stmt $ convertPlayerToRow player
  )

insertMany :: [Player] -> IO ()
insertMany players = commitWithDBConnection
  (\conn -> do
    stmt <- prepare conn $ "INSERT INTO " ++ getTableName ++ " VALUES (?, ?, ?, ?)"
    let playerSqlArrays = map convertPlayerToRow players
    executeMany stmt playerSqlArrays
  )

updateOne :: Player -> IO ()
updateOne updatedPlayer = commitWithDBConnection
  (\conn -> do
      stmt <- prepare conn $ " UPDATE " ++ getTableName ++ "\n \
                         \ SET score=?, rank=?\n \
                         \ WHERE id=?"
      execute stmt [toSql $ score updatedPlayer, toSql $ rank updatedPlayer, toSql $ id_num updatedPlayer]
  )

updateMany :: [Player] -> IO ()
updateMany updatedPlayers = commitWithDBConnection
  (\conn -> do
    stmt <- prepare conn $ "  UPDATE " ++ getTableName ++ "\n \
                            \ SET score=?, rank=?\n \
                            \ WHERE id=?"
    let placeholderValues = map (\updatedPlayer -> [toSql $ score updatedPlayer, toSql $ rank updatedPlayer, toSql $ id_num updatedPlayer])  updatedPlayers
    executeMany stmt placeholderValues
  )

-- TODO: id_name is enough for deletion, but for the sake of it having the same form as other db operations i have used Player -> IO() instead of Int -> IO() -- *subject to change*
deleteOne :: Player -> IO ()
deleteOne player = commitWithDBConnection
  (\conn -> do
    stmt <- prepare conn $ " DELETE FROM " ++ getTableName ++ "\n \
                         \ WHERE id=?"
    execute stmt [toSql $ id_num player]
  )

deleteMany :: [Player] -> IO()
deleteMany players = commitWithDBConnection
  (\conn -> do
    stmt <- prepare conn $ " DELETE FROM " ++ getTableName ++ "\n \
                          \ WHERE id=?"
    let placeholderValues = map (\player -> [toSql $ id_num player]) players
    executeMany stmt placeholderValues
  )

convertPlayerToRow :: Player -> [SqlValue]
convertPlayerToRow player = [toSql $ id_num player, toSql $ username player, toSql $ score player, toSql $ rank player]

convertRowToPlayer :: [SqlValue] -> Player
convertRowToPlayer row = Player (fromSql $ row !! 0 :: Int) (fromSql $ row !! 1) (fromSql $ row !! 2 :: Int) (fromSql $ row !! 3 :: Int)

-- Pagination logic
-- TODO: bracket

getPlayersPerPage :: Int
getPlayersPerPage = 10

calculateNumberOfPages :: IO Int
calculateNumberOfPages = do
  conn <- getDatabaseConnection
  queryResult <- quickQuery' conn ("SELECT count(*) from " ++ getTableName) []   -- The result here will be something like [[x]] where x is the SqlValue number of items/rows in the table
  let playersPerPage = getPlayersPerPage
  let totalNumberOfPlayers = fromSql $ head $ head queryResult :: Int
  let floatDivResult = (fromIntegral totalNumberOfPlayers :: Float) / (fromIntegral playersPerPage :: Float)
  return (ceiling floatDivResult :: Int)

getScoreboardPage :: Int -> IO [Player]
getScoreboardPage targetPageNumber = do
  conn <- getDatabaseConnection
  let offsetBegin = (targetPageNumber - 1) * getPlayersPerPage
  queryResult <- quickQuery' conn (
                                    " SELECT * from " ++ getTableName ++ "\n \
                                    \ LIMIT ? OFFSET ?"
                                  ) [toSql getPlayersPerPage, toSql offsetBegin]
  return $ map convertRowToPlayer queryResult
