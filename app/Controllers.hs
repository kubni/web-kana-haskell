{-# language OverloadedStrings #-}
module Controllers (index, generateHiraganaCharacter, missing) where

-- This is a "trick" that lets us keep the Text type, yet requires us to use T.func_name, so we get best of the both worlds for readability
-- This is done in the first place because Data.Text has a lot of ambiguous conflicts with Prelude
import Data.Text (Text)
import qualified Data.Text as T

import Web.Twain

import Data.Map ((!))
import KanaTables (hiraganaTable) -- TODO: Is it okay to import (!) from KanaTables or should i import Data.Map here as well?


index :: ResponderM a
-- index = send $ html "Hello World!"
index = do
  send $ text $ "Hello world!"

generateHiraganaCharacter = undefined


missing :: ResponderM a
missing = send $ html "Not found..."
