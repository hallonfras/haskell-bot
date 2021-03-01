{-# LANGUAGE OverloadedStrings #-} 
module Poetry (poetry) where

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
{- Poetry
  Represents a poetry with title,author and poetry content lines. 

  title - title of the poetry
  author - the poet
  lines - each line in the poetry
-}
data Poetry = Poetry {
    title :: String
    , author :: String
    , lines :: [String]
    } deriving (Show)

-- defines stringIt for the poetry type
instance Stringable Poetry where
        stringIt (Poetry _ author lines) = parsePoetry lines

-- defines JSON parsing for the poetry type
instance FromJSON Poetry where
        parseJSON (Object v) = do
            title  <- v .: "title"
            author <- v .: "author"
            lines <- v .: "lines"
            return $ Poetry title author lines
        parseJSON _ = mempty
{-  parsePoetry lines
    concats lines into a single string where \n is attached to the end of each former separate string. ("\n" represents a new line)
    RETURNS: A single string where \n is attached to the end of each former seperate string.
    EXAMPLES: parsePoetry ["hello", "how", "you doing?"] == "hello\nhow\nyou doing?\n"
              parsePoetry ["coolnice"] == "cool\nnice\n"
-}

parsePoetry :: [[Char]] -> [Char]
parsePoetry = foldl (\concated' line -> concated' ++ line ++ "\n") "" 

{- getPoetry
     Gets a messagedata containing a poetry
     PRE: --
     RETURNS: messagedata containing a poetry
     SIDE EFFECTS: performs a http request
     EXAMPELS --
  -}
getPoetry :: DiscordHandler (MessageData (Maybe [Poetry]))
getPoetry = do
        json <- apiRequest "https://poetrydb.org/random" ""
        let poetry = Data.Aeson.decode $ BSL.fromStrict json :: Maybe [Poetry]
        toMessageData poetry

{- poetry m
     Sends a discord embed containing a poetry
     PRE: --
     RETURNS: --
     SIDE EFFECTS: perfoms a http request and performs a rest call to the discord api
     EXAMPLES: -- 
  -}
poetry :: Message -> DiscordHandler ()
poetry m = do
        msgdata <- getPoetry
        let 
                poetry = head (fromMaybe $ value msgdata)
                icon = "https://cdn4.iconfinder.com/data/icons/academic-disciplines-basic/64/poetry-512.png"
                t = pack (title poetry ++ " by " ++ author poetry)
        handleMessage m msgdata t icon