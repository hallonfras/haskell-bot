{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad (when, void)
import System.Environment
import Data.String.Builder

import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Concurrent ( threadDelay )
import Data.Text (isPrefixOf, toLower, Text, unpack, pack, isInfixOf, splitAt, splitOn)
import qualified Data.Text.IO as TIO
import Canvas
import Utils
import Joke
import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.List.Split as Split
import Data.Data ( Data )
import qualified Weather

type Command = String 


main :: IO ()
main = do
        loadFile defaultConfig
        startBot

commands :: [(Command, Message -> DiscordHandler ())]
commands = [("joke",dadjoke),("courses",canvCourses),("assignments",canvAssignments),("weather", Weather.handleMessage)]

notFound :: Message -> DiscordHandler ()
notFound m = do
        restCall (R.CreateMessage (messageChannel m) ("command not found"))
        pure ()

findCommand :: [(String, (Message -> DiscordHandler ()) )] -> String -> (Message -> DiscordHandler ())
findCommand list string
        | null list = notFound
        | fst (head list) == string = snd (head list)
        | otherwise = findCommand (tail list) string

startBot :: IO ()
startBot = do 
        -- token <- getEnv "DISCORD_TOKEN"
        userFacingError <- runDiscord $ def 
                { discordToken = pack "ODExOTE1MjYyNDA0MTk4NDUw.YC5JAw.lKLEt2mVBI-6V_kamrEoZUUA0iE", 
                discordOnEvent = eventHandler }
        TIO.putStrLn userFacingError


eventHandler :: Event -> DiscordHandler()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isCommand (messageText m)) $ do
        findCommand commands (tail $ head $ Split.splitOn "-" (removeSpace $ unpack $ messageText m)) $ m
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: Text -> Bool
isCommand text = "!" `isPrefixOf` (toLower text)

