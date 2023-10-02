{-# language OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Web.Twain
import Data.Text as T hiding (foldr, index)
import KanaTables (hiraganaTable, (!))

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
-- index = send $ html "Hello World!"
index = do
  let testString = hiraganaTable ! "vowels_column" !! 0
  send $ text $ T.pack testString

echoName :: ResponderM a
echoName = do
  name <- param "name"
  send $ html $ "Hello, " <> name

missing :: ResponderM a
missing = send $ html "Not found..."
