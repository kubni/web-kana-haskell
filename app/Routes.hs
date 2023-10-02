{-# language OverloadedStrings #-}
module Routes (routes, routeNotFound) where

import Web.Twain (Middleware, Application)
import Web.Twain as Twain (get, notFound)
import Controllers (index, generateHiraganaCharacter, missing)

routes :: [Middleware]
routes =
  [ get "/" index
  , get "/game/generateHiraganaCharacter" generateHiraganaCharacter
  ]

routeNotFound :: Application
routeNotFound = notFound Controllers.missing
