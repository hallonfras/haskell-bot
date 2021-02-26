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

{- main
    loads the .env file using the Dotenv library. Then calls startbot
-}
main = do
        loadFile defaultConfig
        startBot

-- List of commands which the bot can run
commands :: [(String, Message -> DiscordHandler ())]
commands = [("test", test)]

{- test command
    replies "the test function works at least" in the same discord channel where the command was sent
    SIDE EFFECTS: performs a rest call in order to create the reply message
-}
test :: Message -> DiscordHandler ()
test m = do
        restCall (R.CreateMessage (messageChannel m) "the test function works at least")
        pure ()

{- test command
    replies "That command doesnt exist!" in the same discord channel where the command was sent.
    This is used when findCommand doesnt find an appropriate command
    SIDE EFFECTS: performs a rest call in order to create the reply message
-}
notFound :: Message -> DiscordHandler ()
notFound m = do
        restCall (R.CreateMessage (messageChannel m) "That command doesnt exist!")
        pure ()

{- findCommand commandList commandName
     finds the command with name commandName in commandList if none is found it will return the notFound function
     RETURNS: the function which is associated with commandName or notFound if no appropriate command is found
  -}
findCommand :: [(String, (Message -> DiscordHandler ()) )] -> String -> (Message -> DiscordHandler ())
findCommand list string
        | null list = notFound -- base case
        | fst (head list) == string = snd (head list) -- matching command is found
        | otherwise = findCommand (tail list) string -- inductive case

{- startBot
     Library function which starts the main event handling loop for the discord bot. 
     Will print any errors it runs into to the terminal
     SIDE EFFECTS: Prints eventual errors to the terminal
-}
startBot :: IO ()
startBot = do 
        token <- getEnv "DISCORD_TOKEN"
        userFacingError <- runDiscord $ def 
                { discordToken = pack token, 
                discordOnEvent = eventHandler }
        TIO.putStrLn userFacingError

{- eventHandler event
     This function is ran by the main event handling loop once an event is received.
     If the event was for creating a message and is a command the bot will attempt to run the command from
     the list of available commands.
     SIDE EFFECTS: runs a command
-}
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isCommand (messageText m)) $ do
        findCommand commands (tail (unpack (messageText m))) $ m
    _ -> pure ()

-- checks if the message is from a bot
fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

-- checks if a message is a command or not. (commands are prefixed with !)
isCommand :: Text -> Bool
isCommand text = "!" `isPrefixOf` (toLower text)

