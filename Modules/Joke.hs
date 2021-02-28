{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Joke where

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

newtype Joke = Joke {joke :: String} deriving (Show)

instance Stringable Joke where
        stringIt (Joke j) = j

instance FromJSON Joke where
        parseJSON (Object v) = do
                joke  <- v .: "joke"
                return (Joke {joke = joke})
        parseJSON _ = mempty


{-  jokeRequest
    Requests a dad joke in JSON FORMAT from https://icanhazdadjoke.com/ using GET.
    RETURNS: A random "joke" and its corresponding "id" and "status"
    EXAMPLES: jokeRequest ==
            {"id: R7UfaahVfFd,
              joke: My dog used to chase people on a bike a lot. It got so bad I had to take his bike away.,
              status: 200"}
-}

jokeRequest :: DiscordHandler S8.ByteString
jokeRequest = do
        let request = setRequestMethod "GET"
                $ setRequestHeader "Accept" ["application/json"]
                $ "https://icanhazdadjoke.com/"           
        response <- httpBS request
        return (getResponseBody response)

{-  getJoke
    Returns a messageData containing the joke taken from https://icanhazdadjoke.com/ using APIrequest
    RETURNS: A Maybe of the joke contained within a messageData
    EXAMPLES: getJoke == (Msg (Maybe (Joke joke)) Void)
-}

getJoke :: DiscordHandler (MessageData (Maybe Joke))
getJoke = do
        json <- jokeRequest
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