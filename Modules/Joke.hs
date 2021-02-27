{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Joke where

import Data.Aeson

import Discord
import Discord.Types
import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL

data Joke = Joke {joke :: String} deriving (Show)

instance FromJSON Joke where
        parseJSON (Object v) = do
                joke  <- v .: "joke"
                return (Joke {joke = joke})
        parseJSON _ = mempty

getJoke :: DiscordHandler (Maybe Joke)
getJoke = do
        response <- jokerequest
        let 
                json = getResponseBody response
                joke = Data.Aeson.decode $ BSL.fromStrict json :: Maybe Joke
        return (joke)

jokerequest :: DiscordHandler (Response S8.ByteString)
jokerequest = do
        let request = setRequestMethod "GET"
                $ setRequestHeader "Accept" ["application/json"]
                $ "https://icanhazdadjoke.com/"           
        httpBS request

fromMaybeJoke :: Maybe Joke -> String 
fromMaybeJoke (Just (Joke j)) = j