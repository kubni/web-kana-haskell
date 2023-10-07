module RandomGenerators (generateRandomInteger) where

import System.Random as Random

createRandomIntGen :: Int -> StdGen
createRandomIntGen = mkStdGen

getRandomSeed :: IO Int
getRandomSeed = do
  fst . Random.random <$> getStdGen

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
