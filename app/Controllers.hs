{-# language OverloadedStrings #-}

module Controllers (index, generateHiraganaCharacter, missing) where

import KanaTables (hiraganaTable)
import RandomGenerators

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map ((!), keys)
import Web.Twain
import Control.Monad.IO.Class (liftIO)





--------- Actual controllers -------------
index :: ResponderM a
index = send $ html "Hello World!"

generateHiraganaCharacter :: ResponderM a
generateHiraganaCharacter = do
  let columns = keys hiraganaTable
  randomColumnInt <- liftIO $ generateRandomInteger 0 (length columns - 1)

  let randomColumn = hiraganaTable ! (columns !! randomColumnInt)
  randomCharInt <- liftIO $ generateRandomInteger 0 (length randomColumn - 1)

  let randomChar = randomColumn !! randomCharInt
  send $ text $ T.pack randomChar


missing :: ResponderM a
missing = send $ html "Not found..."
------------------------------------------

---------- Helper functions --------------
------------------------------------------
