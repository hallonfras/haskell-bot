{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when)
import Control.Concurrent ( threadDelay )
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do userFacingError <- runDiscord $ def
                                            { discordToken = "ODExOTE1MjYyNDA0MTk4NDUw.YC5JAw.9padpF37OsoFpcimtC7mYNbeO-c"
                                            , discordOnEvent = eventHandler }
                     TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
               _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
               _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower