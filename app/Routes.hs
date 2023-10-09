{-# language OverloadedStrings #-}
module Routes (routes, routeNotFound) where

import Web.Twain (Middleware, Application)
import Web.Twain as Twain (get, notFound)
import Controllers

routes :: [Middleware]
routes =
  [ get "/" index
  , get "/game/generateHiraganaCharacter" generateHiraganaCharacter
  , get "/game/generateKatakanaCharacter" generateKatakanaCharacter
  ]

routeNotFound :: Application
routeNotFound = notFound missing
