{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when, void)
import System.Environment

import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Concurrent ( threadDelay )
import Data.Text (isPrefixOf, toLower, Text, unpack, pack)
import qualified Data.Text.IO as TIO

import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

main = do
        loadFile defaultConfig
        startBot

commands :: [(String, Message -> DiscordHandler ())]
commands = [("zhanwei", zhanwei)]

zhanwei :: Message -> DiscordHandler ()
zhanwei m = do
        restCall (R.CreateMessage (messageChannel m) "Yes this looks good")
        pure ()

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

