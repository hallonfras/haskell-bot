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
import Joke
import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.List.Split as Split
import Data.Data ( Data )

type Command = String 
type Argument = String
import qualified Weather

main :: IO ()
main = do
        loadFile defaultConfig
        startBot

commands :: [(Command, Message -> DiscordHandler ())]
commands = [("joke",dadjoke),("courses",canvCourses),("assignments",canvAssignments),("weather", Weather.handleMessage)]

canvAssignments :: Message -> DiscordHandler ()
canvAssignments m = do
        let
                args = tail $ Split.splitOn "-" (removeSpace $ unpack $ messageText m)
                courseid = args !! 0 -- courseid is the first and only argument ex. (!assignments -3085)
        msgdata <- getAssignments "canvas token" courseid
        dataListToMessage m msgdata

canvCourses :: Message -> DiscordHandler ()
canvCourses m = do
        msgdata <- getCourses "canvas token"
        dataListToMessage m msgdata

dataListToMessage :: (Data a,Show a) => Message -> MessageData [a] -> DiscordHandler ()
dataListToMessage m msgdata = do
        if Canvas.error msgdata
        then do 
                sendMessage' m (errorMsg msgdata)
        else do 
                str <- dataListToString (value msgdata)
                sendMessage' m str

dataToMessage :: (Data a,Show a) => Message -> MessageData a -> DiscordHandler ()
dataToMessage m msgdata = do
        if Canvas.error msgdata
        then do 
                sendMessage' m (errorMsg msgdata)
        else do 
                str <- dataToString (value msgdata)
                sendMessage' m str

sendMessage' :: Message -> String -> DiscordHandler ()
sendMessage' m str = do
        restCall (R.CreateMessage (messageChannel m) (pack str))
        pure()

dadjoke :: Message -> DiscordHandler ()
dadjoke m = do
        joke' <- getJoke
        restCall (R.CreateMessage (messageChannel m) (pack (fromMaybeJoke joke')))
        pure ()




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
                { discordToken = pack "discord token", 
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

argumentExists :: Text -> Bool
argumentExists = isInfixOf "-"


removeSpace xs = foldl (\clean char -> if char == ' ' then clean else clean ++ [char]  ) "" xs