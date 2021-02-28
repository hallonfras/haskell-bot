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

getPoetry :: DiscordHandler (MessageData (Maybe [Poetry]))
getPoetry = do
        json <- apiRequest "https://poetrydb.org/random" ""
        let poetry = Data.Aeson.decode $ BSL.fromStrict json :: Maybe [Poetry]
        toMessageData poetry

poetry :: Message -> DiscordHandler ()
poetry m = do
        msgdata <- getPoetry
        let 
                poetry = (fromMaybe $ value msgdata) !! 0
                icon = "https://cdn4.iconfinder.com/data/icons/academic-disciplines-basic/64/poetry-512.png"
                t = (pack (title poetry ++ " by " ++ author poetry))
        handleMessage m msgdata t icon
                