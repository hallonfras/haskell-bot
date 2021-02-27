module Utils where

import Data.Data
import Data.Aeson
import Data.Aeson.Types

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL

import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)


class Stringable a where
    stringIt :: a -> String

instance Stringable a => Stringable [a] where
    stringIt xs = foldl (\ str x -> str ++ stringIt x ++ "\n\n") "" xs

data MessageData a = Msg {
    value :: a,
    error :: Error
} 
data Error = Void | E String deriving (Eq)

getError :: Error -> String
getError (E s) = s

maybeToString :: Show a => Maybe a -> Maybe String
maybeToString (Just i) = Just (show i)
maybeToString Nothing = Nothing

jsonToMessageData :: FromJSON a => S8.ByteString -> DiscordHandler (MessageData [a])
jsonToMessageData json = do
    let 
        value = Data.Aeson.decode $ BSL.fromStrict json
    if apiFail value
    then do return (Msg [] (E "Failed to retrieve data"))
    else do return (Msg (fromMaybe value) Void)

apiFail :: Maybe a -> Bool 
apiFail Nothing = True
apiFail _ = False

fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a

removeSpace xs = foldl (\clean char -> if char == ' ' then clean else clean ++ [char]  ) "" xs

handleMessage :: (Data a,Show a,Stringable a) => Message -> MessageData a -> Text -> Text -> DiscordHandler ()
handleMessage m msgdata title icon = do
        if Utils.error msgdata /= Void
        then do 
                sendEmbed m (pack $ getError $ Utils.error msgdata) title icon
        else do 
                let txt = pack $ stringIt (value msgdata)
                sendEmbed m txt title icon

sendEmbed :: Message -> Text -> Text -> Text -> DiscordHandler ()
sendEmbed m s title icon = do
        restCall (R.CreateMessageEmbed (messageChannel m) (pack "") $
            def { createEmbedTitle = title,
             createEmbedDescription = s, 
             createEmbedThumbnail = Just $ CreateEmbedImageUrl $ icon
            })
        pure ()