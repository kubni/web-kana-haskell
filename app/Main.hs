module Main where

import Network.Wai.Handler.Warp (run)

import Routes (routes, routeNotFound)
import Database.HDBC.ODBC as H (connectODBC)

main :: IO ()
main = do
  conn <- H.connectODBC "DSN=MariaDBTest"

  print "Server is starting at localhost:8080. Press C-c to exit."
  run 8080 $
    foldr ($) routeNotFound routes
