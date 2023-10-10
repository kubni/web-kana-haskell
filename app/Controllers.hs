{-# language OverloadedStrings #-}

module Controllers (
  index,
  generateHiraganaCharacter, generateKatakanaCharacter,
  checkAnswer,
  getScoreboardPage,
  checkUsername,
  missing
  ) where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Map ((!), keys)
import Web.Twain
import Control.Monad.IO.Class (liftIO)


import KanaTables (hiraganaTable, katakanaTable, romajiTable)
import RandomGenerators
import qualified Models as M


--------- Actual controllers -------------
missing :: ResponderM a
missing = send $ html "Not found..."


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


-- TODO: Separate romaji tables for hiragana and katakana?
checkAnswer :: ResponderM a
checkAnswer = do
  userAnswerRomaji <- param "answer"
  correctAnswerCharacter <- param "correctAnswer"
  let correctAnswerRomaji = romajiTable ! correctAnswerCharacter
  let response = if userAnswerRomaji == correctAnswerRomaji then "correct" else "incorrect"
  send $ text $ T.pack response

getScoreboardPage :: ResponderM a
getScoreboardPage = do
  targetPage <- param "pageNumber"
  playersOnThisPage <- liftIO $ M.getScoreboardPage $ read targetPage
  send $ json playersOnThisPage

------------------------------------------

checkUsername :: ResponderM a
checkUsername = do
  targetUsername <- param "username"
  isUsernameValid <- liftIO $ M.checkIfUsernameAlreadyExists targetUsername
  let response = show isUsernameValid
  send $ text $ T.pack response





-- TODO: Send text responses as json instead?
