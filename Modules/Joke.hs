{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Joke (
        dadjoke,
        Joke (..)

) where

import Data.Aeson

import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)
import Utils
{- Joke
  Represents a joke contained in a string

  joke - the joke in a string
-}
newtype Joke = Joke {joke :: String} deriving (Show, Eq)

-- defines stringIt for the joke type
instance Stringable Joke where
        stringIt (Joke j) = j

-- defines JSON parsing for the joke type
instance FromJSON Joke where
        parseJSON (Object v) = do
                joke  <- v .: "joke"
                return (Joke {joke = joke})
        parseJSON _ = mempty

{-  getJoke
    Returns a messageData containing the joke taken from https://icanhazdadjoke.com/ using APIrequest
    RETURNS: A Maybe of the poetry contained within a messageData
    EXAMPLES: getPoetry == (Msg (Poetry (Maybe (title author lines])) Void)
-}

getJoke :: DiscordHandler (MessageData (Maybe Joke))
getJoke = do
        json <- apiRequest "https://icanhazdadjoke.com/" ""
        let joke = Data.Aeson.decode $ BSL.fromStrict json
        toMessageData joke

{-  dadjoke m
    Creates an embedded discord message, titled "Dad joke", containing the joke alongside an icon.
    SIDE EFFECT: Performs an http request
-}

dadjoke :: Message -> DiscordHandler ()
dadjoke m = do
        msgdata <- getJoke
        let
                title = "Dad joke"
                icon = "http://assets.stickpng.com/images/586294223796e30ac446872f.png"
        handleMessage m msgdata title icon