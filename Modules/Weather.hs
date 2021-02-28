{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Weather where 

import Control.Monad

import           Data.Aeson
import           Data.Text
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple
import System.Environment
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Utils



data Weather = Weather { 
    description :: String
  , temperature :: Float
  , icon        :: String
} deriving (Show)


instance FromJSON Weather where
    parseJSON (Object obj) = do
        weatherObj <- obj .: "weather"
        let actualWeatherObj = Prelude.head weatherObj
        desc <- actualWeatherObj .: "description"
        ic <- actualWeatherObj .: "icon"
        dataObj <- obj .: "main"
        temp <- dataObj .: "temp"
        return (Weather { description = desc, temperature = temp, icon = ic })
    parseJSON _ = mempty
    
getWeather :: DiscordHandler (Maybe Weather)
getWeather  = do
    let api = "https://api.openweathermap.org/data/2.5/weather?q=Uppsala&appid=ce3a449055d96d97c82166fff5434393"
    json <- apiRequest api ""
    let 
        weather = Data.Aeson.decode $ BSL.fromStrict json :: Maybe Weather
    return weather

weatherToString :: Maybe Weather -> String
weatherToString (Just (Weather desc temp _)) = "The weather in Uppsala is " ++ desc ++ " with a temperature of " ++ show (temp - 273.15)
weatherToString Nothing = ""

weatherIcon :: Maybe Weather -> String
weatherIcon (Just (Weather _ _ icon)) = "http://openweathermap.org/img/wn/" ++ icon ++ "@2x.png"
weatherIcon Nothing = ""

handleMessage :: Message -> DiscordHandler ()
handleMessage m = do
    weather <- getWeather
    let icon = pack (weatherIcon weather)
    let string = pack (weatherToString weather)
    _ <- restCall (R.CreateMessageEmbed (messageChannel m) "" $
            def { createEmbedTitle = "CoolNiceWeather",
             createEmbedDescription = string, 
             createEmbedThumbnail = Just $ CreateEmbedImageUrl icon
            })
    return()

