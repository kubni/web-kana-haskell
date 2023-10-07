module Main where

import Network.Wai.Handler.Warp (run)

import Routes (routes, routeNotFound)
import Models (insertOne)
main :: IO ()
main = do
  print "Server is starting at localhost:8080. Press C-c to exit."
  insertOne
  run 8080 $
    foldr ($) routeNotFound routes










{-
create table if not exists Polaznik (
       id int primary key,
       ime varchar(50),
       prezime varchar(50),
       status varchar(50),
       godine int,
       id_skole int
);

-}
