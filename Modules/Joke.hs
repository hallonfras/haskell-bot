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

data Joke = Joke {joke :: String} deriving (Show)

instance Stringable Joke where
        stringIt (Joke j) = j

instance FromJSON Joke where
        parseJSON (Object v) = do
                joke  <- v .: "joke"
                return (Joke {joke = joke})
        parseJSON _ = mempty

jokeRequest :: DiscordHandler (S8.ByteString)
jokeRequest = do
        let request = setRequestMethod "GET"
                $ setRequestHeader "Accept" ["application/json"]
                $ "https://icanhazdadjoke.com/"           
        response <- httpBS request
        return (getResponseBody response)

getJoke :: DiscordHandler (MessageData (Maybe Joke))
getJoke = do
        json <- jokeRequest
        let joke = Data.Aeson.decode $ BSL.fromStrict json
        toMessageData joke

fromMaybeJoke :: Maybe Joke -> String 
fromMaybeJoke (Just (Joke j)) = j

dadjoke :: Message -> DiscordHandler ()
dadjoke m = do
        msgdata <- getJoke
        let
                title = "Joke á la David"
                icon = "http://assets.stickpng.com/images/586294223796e30ac446872f.png"
        handleMessage m msgdata title icon