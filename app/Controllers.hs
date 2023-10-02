{-# language OverloadedStrings #-}
module Controllers (index, generateHiraganaCharacter, missing) where

import KanaTables (hiraganaTable)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map ((!), keys)
import Web.Twain
import System.Random as Random
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
createRandomIntGen :: Int -> StdGen
createRandomIntGen = mkStdGen

getRandomSeed :: IO Int
getRandomSeed = do
  randomSrc <- getStdGen
  return $ fst $ Random.random randomSrc

generateRandomInteger :: Int -> Int -> IO Int
generateRandomInteger a b = do
  randomSeed <- getRandomSeed
  let randomIntGen = createRandomIntGen randomSeed
  return $ head $ Random.randomRs (a, b) randomIntGen





-- TODO: The following is equivalent
-- generateRandomInteger = curry randomRIO

-- curry :: ((a, b) -> c) -> a -> b -> c
-- randomRIO :: (Random a, MonadIO m) => (a, a) -> m a  -- Returns a value of type Random that is encapsulated inside a MonadIO monad (or some other monad that instances MonadIO). It utilizes global pseudo-random number gen.
-- curry randomRIO gives a function :: a -> a -> m a
-- In our case with integers, that becomes Int -> Int -> IO Int
------------------------------------------
