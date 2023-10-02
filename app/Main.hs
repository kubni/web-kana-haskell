{-# language OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Web.Twain


main :: IO ()
main = do
  print "Server is starting at localhost:8080. Press C-c to exit."
  run 8080 $
    foldr ($) (notFound missing) routes


-- TODO: Move this to `routes` module
routes :: [Middleware]
routes =
  [ get "/" index
  , get "/echo/:name" echoName
  ]


-- TODO: Move these functions to `controllers` module
index :: ResponderM a
index = send $ html "Hello World!"

echoName :: ResponderM a
echoName = do
  name <- param "name"
  send $ html $ "Hello, " <> name

missing :: ResponderM a
missing = send $ html "Not found..."
