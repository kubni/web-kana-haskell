{-# language OverloadedStrings #-}
module Controllers (index, generateHiraganaCharacter, missing) where

import Data.Map ((!))
import KanaTables (hiraganaTable)

-- This is a "trick" that lets us keep the Text type, yet requires us to use T.func_name, so we get best of the both worlds for readability
-- This is done in the first place because Data.Text has a lot of ambiguous conflicts with Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Web.Twain
import System.Random as Random
import Control.Monad.IO.Class ( MonadIO(liftIO) )




--------- Actual controllers -------------
index :: ResponderM a
-- index = send $ html "Hello World!"
index = do
  send $ text "Hello world!"

generateHiraganaCharacter = undefined


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

------------------------------------------
