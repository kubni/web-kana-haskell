module Main where

import Network.Wai.Handler.Warp (run)

import Routes (routes, routeNotFound)

main :: IO ()
main = do
  print "Server is starting at localhost:8080. Press C-c to exit."
  run 8080 $
    foldr ($) routeNotFound routes
