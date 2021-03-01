{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.Data
import Data.Aeson
import Data.Aeson.Types

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple

import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)
import Data.Maybe


class Stringable a where
    stringIt :: a -> String

--stringIt for lists of data
instance Stringable a => Stringable [a] where
    stringIt xs = foldl (\ str x -> str ++ stringIt x ++ "\n\n") "" xs --format for lists of data

data MessageData a = Msg {
    value :: a,
    error :: Error
} 
data Error = Void | E String deriving (Eq)

getError :: Error -> String
getError (E s) = s

{- apiRequest source token
     Sends api request to source and returns a json response
     PRE: --
     RETURNS: a json response
     SIDE EFFECTS: performs a http request
     EXAMPELS --
  -}

apiRequest :: String -> [Char] -> DiscordHandler (S8.ByteString)
apiRequest source token = do
    request <- parseRequest source
    let request' = setRequestMethod "GET"
                    $ setRequestHeader "Authorization" [S8.pack ("Bearer " ++ token)]
                    $ setRequestHeader "Accept" ["application/json"]
                    $ request
    response <- httpBS request'
    return (getResponseBody response)

maybeToString :: Show a => Maybe a -> Maybe String
maybeToString (Just i) = Just (show i)
maybeToString Nothing = Nothing

toMessageData :: FromJSON a => Maybe a -> DiscordHandler (MessageData (Maybe a))
toMessageData value = do
    if isNothing value -- if api failed to retrieve data
    then do return (Msg Nothing (E "Failed to retrieve data"))
    else do return (Msg value Void)

fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a

--removes spaces from string
removeSpace :: Foldable t => t Char -> [Char]
removeSpace xs = foldl (\clean char -> if char == ' ' then clean else clean ++ [char]  ) "" xs

--handles the message B) 
handleMessage :: (Show a,Stringable a) => Message -> MessageData (Maybe a) -> Text -> Text -> DiscordHandler ()
handleMessage m msgdata title icon = do
        if Utils.error msgdata /= Void
        then do
                let 
                    error_title = pack "Error 404"
                    error_icon = pack "https://cdn0.iconfinder.com/data/icons/shift-free/32/Error-512.png"
                sendEmbed m (pack $ getError $ Utils.error msgdata) error_title error_icon 
        else do 
                let txt = pack $ stringIt $ Utils.fromMaybe (value msgdata)
                sendEmbed m txt title icon

--creates and sends embed
sendEmbed :: Message -> Text -> Text -> Text -> DiscordHandler ()
sendEmbed m s title icon = do
        restCall (R.CreateMessageEmbed (messageChannel m) (pack "") $
            def { createEmbedTitle = title,
             createEmbedDescription = s, 
             createEmbedThumbnail = Just $ CreateEmbedImageUrl $ icon
            })
        pure ()

hyperref :: [Char] -> [Char] -> [Char]
hyperref text url = "[" ++ text ++ "]" ++ "(" ++ url ++")"