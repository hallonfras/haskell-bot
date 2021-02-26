{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad (when, void)
import System.Environment
import Data.String.Builder

import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Concurrent ( threadDelay )
import Data.Text (isPrefixOf, toLower, Text, unpack, pack)
import qualified Data.Text.IO as TIO

import Canvas hiding (test)

import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import GHC.IO (unsafePerformIO)


main = do
        loadFile defaultConfig
        startBot

commands :: [(String, Message -> DiscordHandler ())]
commands = [("zhanwei", zhanwei),("test",test)]


zhanwei :: Message -> DiscordHandler ()
zhanwei m = do
        restCall (R.CreateMessage (messageChannel m) "Yes this looks good")
        pure ()

test :: Message -> DiscordHandler ()
test m = do
        restCall (R.CreateMessage (messageChannel m) (pack mystring))
        pure ()




mystring = build $ do
    "id : 1"
    "name : 2"
    "test : 3"

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
  --      token <- getEnv "DISCORD_TOKEN"
        void $ runDiscord $ def 
                { discordToken = pack "discord token", 
                discordOnEvent = botEventHandler }

messages :: DiscordHandle -> Message -> IO()
messages dis receivedMessage =

        pure ()


botEventHandler :: DiscordHandle -> Event -> IO ()
botEventHandler dis event =
    case event of
        MessageCreate m -> messages dis m
        _               -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: Text -> Bool
isCommand text = "!" `isPrefixOf` (toLower text)

