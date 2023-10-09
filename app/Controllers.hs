{-# language OverloadedStrings #-}

module Controllers (
  index,
  generateHiraganaCharacter, generateKatakanaCharacter,
  missing
  ) where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Map ((!), keys)
import Web.Twain
import Control.Monad.IO.Class (liftIO)


import KanaTables (hiraganaTable, katakanaTable)
import RandomGenerators
--import Models ( Player(Player), insertOne, insertMany)  -- Player(Player) means that we import the Player constructor for type Player


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

generateKatakanaCharacter :: ResponderM a
generateKatakanaCharacter = do
  let columns = keys katakanaTable
  randomColumnInt <- liftIO $ generateRandomInteger 0 (length columns - 1)

  let randomColumn = katakanaTable ! (columns !! randomColumnInt)
  randomCharInt <- liftIO $ generateRandomInteger 0 (length randomColumn - 1)

  let randomChar = randomColumn !! randomCharInt
  send $ text $ T.pack randomChar

missing :: ResponderM a
missing = send $ html "Not found..."
------------------------------------------

---------- Helper functions --------------
------------------------------------------
