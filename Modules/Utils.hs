{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.Data
import Data.Aeson
import Data.Aeson.Types

import Data.Maybe

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple

import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)
import Data.Maybe

{-
stringIt formats data to a string that can be displayed in discord
-}
class Stringable a where
    stringIt :: a -> String

--stringIt for lists of data
instance Stringable a => Stringable [a] where
    stringIt xs = foldl (\ str x -> str ++ stringIt x ++ "\n\n") "" xs

{- MessageData

Represents a message containing a value to send and a potential error.

value - data to be sent
error - potential error
-}
data MessageData a = Msg {
    value :: a,
    error :: Error
} 
{- Error

Represents an error.

Void == no error
E String == error with string message
-}
data Error = Void | E String deriving (Eq)

{-  getError err
    Gets the error message contained in err
    RETURNS: the error message contained in err
    EXAMPLES: getError (E "hamzi") == "hamzi" 
-}
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

{- maybeToString maybe
     Converts the value contained in maybe to a string
     PRE: the value contained in maybe can be converted to a string
     RETURNS: Nothing || Just (value contained in maybe as a string)
     EXAMPLES: maybeToString (Just 5) == Just "5"
  -}

maybeToString :: Show a => Maybe a -> Maybe String
maybeToString (Just i) = Just (show i)
maybeToString Nothing = Nothing

{- toMessageData data
     Returns an error message if data contains Nothing otherwise it returns data as a message
     RETURNS: A messageData containing either a message or error
     EXAMPLES: toMessageData Nothing == (Msg Nothing (E "Failed to retrieve data"))
               toMessageData (Just 5) == (Msg (Just 5) Void)
  -}
toMessageData :: FromJSON a => Maybe a -> DiscordHandler (MessageData (Maybe a))
toMessageData value = do
    if isNothing value -- if api failed to retrieve data
    then do return (Msg Nothing (E "Failed to retrieve data"))
    else do return (Msg value Void)
    
{- fromMaybe maybe
     Extracts the value from a maybe
     RETURNS: the value contained within maybe
     EXAMPLES: (Just 5) == 5
		    (Just “cool”) == “cool”
-}
fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a

{- removeSpace string
     trims spaces from a string
     RETURNS: string but with all spaces removed
     EXAMPLES: removeSpace “a cool and nice string” == “acoolandnicestring”
-}

removeSpace :: Foldable t => t Char -> [Char]
removeSpace xs = foldl (\clean char -> if char == ' ' then clean else clean ++ [char]  ) "" xs

{- handleMessage message msgdata embedTitle embedIcon
     checks if the msgdata contains an error then sends either an error embed or an embed 
	containing the message data as well as title and icon
     SIDE EFFECTS: Performs a restcall to the discord api in order to send the embed
 -}

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

{- sendEmbed message string title icon
     helper function whichformats and sends a discord embed in the same channel as the input message
     SIDE EFFECTS: Performs a restcall to the discord api in order to send the embed
  -}

sendEmbed :: Message -> Text -> Text -> Text -> DiscordHandler ()
sendEmbed m s title icon = do
        restCall (R.CreateMessageEmbed (messageChannel m) (pack "") $
            def { createEmbedTitle = title,
             createEmbedDescription = s, 
             createEmbedThumbnail = Just $ CreateEmbedImageUrl $ icon
            })
        pure ()

{- hyperref text url
     creates a discord hyperref of text containing url
     RETURNS: a discord hyperref of text containing url
     EXAMPLES: hyperref "google" "www.google.com" == "[google](www.google.com)"
  -}
hyperref :: [Char] -> [Char] -> [Char]
hyperref text url = "[" ++ text ++ "]" ++ "(" ++ url ++")"