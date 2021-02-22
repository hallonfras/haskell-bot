{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when)
import Control.Concurrent ( threadDelay )
import Data.Text (isPrefixOf, toLower, Text, unpack)
import qualified Data.Text.IO as TIO

import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R



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



-- | Replies "pong" to every message that starts with "ping"
startBot :: IO ()
startBot = do 
        userFacingError <- runDiscord $ def 
                { discordToken = "#####", 
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

