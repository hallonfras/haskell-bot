{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when, void)
import System.Environment

import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Concurrent ( threadDelay )
import Data.Text (isPrefixOf, toLower, Text, unpack, pack)
import qualified Data.Text.IO as TIO
import Data.Aeson

import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL

data MessageData a = MessageData a Int deriving (Show)

data Joke = Joke {joke :: String}

instance FromJSON Joke where
        parseJSON (Object v) = do
                joke  <- v .: "joke"
                return (Joke {joke = joke})
        parseJSON _ = mempty

main = do
        loadFile defaultConfig
        startBot


commands :: [(String, Message -> DiscordHandler ())]
commands = [("joke", dadjoke)]

dadjoke :: Message -> DiscordHandler ()
dadjoke m = do
        msgdata <- liftIO $ getJoke
        restCall (R.CreateMessage (messageChannel m) (packJoke (msgdata)))


getJoke :: IO (MessageData (Maybe Joke))
getJoke = do
        response <- jokerequest
        let 
                json = getResponseBody response
                joke' = Data.Aeson.decode $ BSL.fromStrict json :: Maybe Joke
        return (MessageData joke' (getResponseStatusCode response))

jokerequest = do
        let request = setRequestMethod "GET"
                $ setRequestHeader "Accept" ["application/json"]
                $ "https://icanhazdadjoke.com/"           
        httpBS request  

packJoke (MessageData (Just (Joke {joke = j})) _ ) = pack j

notFound :: Message -> DiscordHandler ()
notFound m = do
        restCall (R.CreateMessage (messageChannel m) "That command doesnt exist!")
        pure ()

findCommand :: [(String, (Message -> DiscordHandler ()) )] -> String -> (Message -> DiscordHandler ())
findCommand list string
        | null list = notFound
        | fst (head list) == string = snd (head list)
        | otherwise = findCommand (tail list) string 


startBot :: IO ()
startBot = do 
        token <- getEnv "DISCORD_TOKEN"
        userFacingError <- runDiscord $ def 
                { discordToken = pack token, 
                discordOnEvent = eventHandler }
        TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isCommand (messageText m)) $ do
        findCommand commands (tail (unpack (messageText m))) $ m
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: Text -> Bool
isCommand text = "!" `isPrefixOf` (toLower text)

