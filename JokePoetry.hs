{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module JokePoetry where

import Control.Monad
import Data.Aeson

import Discord
import Discord.Types
import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL

newtype Joke = Joke {joke :: String} deriving (Show)

data Poetry = Poetry {
    title :: String
    , author :: String
    , lines :: [String]} deriving (Show)

instance FromJSON Joke where
        parseJSON (Object v) = do
                joke  <- v .: "joke"
                return (Joke {joke = joke})
        parseJSON _ = mempty

instance FromJSON Poetry where
        parseJSON (Object v) = do
            title  <- v .: "title"
            author <- v .: "author"
            lines <- v .: "lines"
            return $ Poetry title author lines
        parseJSON _ = mempty

getJoke :: DiscordHandler (Maybe Joke)
getJoke = do
        response <- jokerequest
        let 
                json = getResponseBody response
                joke = Data.Aeson.decode $ BSL.fromStrict json :: Maybe Joke
        return joke

getPoetry :: DiscordHandler (Maybe Poetry)
getPoetry = do
        response2 <- poetryrequest
        let 
                json = getResponseBody response2
                poetry = Data.Aeson.decode $ BSL.fromStrict json :: Maybe Poetry
        return poetry


jokerequest :: DiscordHandler (Response S8.ByteString)
jokerequest = do
        let jokerequest' = setRequestMethod "GET"
                $ setRequestHeader "Accept" ["application/json"]
                $ "https://icanhazdadjoke.com/"      
        httpBS jokerequest'

poetryrequest :: DiscordHandler (Response S8.ByteString)
poetryrequest = do
        let poetryrequest' = setRequestMethod "GET"
                $ setRequestHeader "Accept" ["application/json"]
                $ "https://poetrydb.org/random"         
        httpBS poetryrequest'

fromMaybeJoke :: Maybe Joke -> String 
fromMaybeJoke (Just (Joke j)) = j

fromMaybePoetry :: Maybe [Poetry] -> String 
fromMaybePoetry Nothing = ""
fromMaybePoetry (Just (Poetry title author lines)) = "The following poem is called: " ++ title ++ "by" ++ author ++ (concat $ lines)

