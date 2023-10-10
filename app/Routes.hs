{-# language OverloadedStrings #-}
module Routes (routes, routeNotFound) where

import Web.Twain (Middleware, Application)
import Web.Twain as Twain (get, post, notFound)
import Controllers

routes :: [Middleware]
routes =
  [ get "/" index
  , get "/game/generateHiraganaCharacter" generateHiraganaCharacter
  , get "/game/generateKatakanaCharacter" generateKatakanaCharacter
  , post "/game/checkAnswer" checkAnswer
  , post "/game/getScoreboardPage/:pageNumber" getScoreboardPage
  , post "/game/checkUsername/:username" checkUsername
  ]

routeNotFound :: Application
routeNotFound = notFound missing
