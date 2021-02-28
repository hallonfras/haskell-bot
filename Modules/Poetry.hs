{-# LANGUAGE OverloadedStrings #-} 
module Poetry where

import Data.Aeson

import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
import Utils

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)

data Poetry = Poetry {
    title :: String
    , author :: String
    , lines :: [String]
    } deriving (Show)

instance Stringable Poetry where
        stringIt (Poetry _ author lines) = parsePoetry lines

{-  parsePoetry txt
    Takes an array and converts it into a single string where \n is attached to the end of each former seperate string. ("\n" is the command for new row in discord)
    RETURNS: A single string where \n is attached to the end of each former seperate string.
    EXAMPLES: parsePoetry ["hello", "how", "you doing?"] == "hello\nhow\nyou doing?"
              parsePoetry ["coolnice"] == "coolnice"
-}
parsePoetry :: [[Char]] -> [Char]
parsePoetry [] = []
parsePoetry [txt] = txt
parsePoetry (a:b:c) = a ++ "\n" ++ b ++ "\n" ++ parsePoetry c 

instance FromJSON Poetry where
        parseJSON (Object v) = do
            title  <- v .: "title"
            author <- v .: "author"
            lines <- v .: "lines"
            return $ Poetry title author lines
        parseJSON _ = mempty

{-  getJoke
    Returns a messageData containing the joke taken from https://icanhazdadjoke.com/ using APIrequest
    RETURNS: A Maybe of the poetry contained within a messageData
    EXAMPLES: getPoetry == (Msg (Poetry (Maybe (title author lines])) Void)
-}

getPoetry :: DiscordHandler (MessageData (Maybe [Poetry]))
getPoetry = do
<<<<<<< HEAD
        json <- apirequest "https://poetrydb.org/random" ""
        let poetry = Data.Aeson.decode $ BSL.fromStrict json :: Maybe [Poetry]
        toMessageData poetry

{-  poetry m
    Creates an embedded discord message, which title is the poems title aswell as its author, containing the poem alongside an icon.
    SIDE EFFECT: Performs an http request
-}

=======
        json <- apiRequest "https://poetrydb.org/random" ""
        let poetry = Data.Aeson.decode $ BSL.fromStrict json :: Maybe [Poetry]
        toMessageData poetry

>>>>>>> main
poetry :: Message -> DiscordHandler ()
poetry m = do
        msgdata <- getPoetry
        let 
                poetry = head (fromMaybe $ value msgdata)
                icon = "https://cdn4.iconfinder.com/data/icons/academic-disciplines-basic/64/poetry-512.png"
                t = pack (title poetry ++ " by " ++ author poetry)
        handleMessage m msgdata t icon

