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

import qualified Utils

 {- A representation of the local weather
     Contains a string describing the weather a Float for the temperature in degrees celsius and 
     a string for the webaddress to an appropriate icon.
-}
data Weather = Weather { 
    description :: String
  , temperature :: Float
  , icon        :: String
} deriving (Show)

-- Defines how the weather type is converted to string
-- (yes we could just have used show but this is cooler)
instance Utils.Stringable Weather where
    stringIt (Weather desc temp _) = "The weather in Uppsala is " ++ desc ++ " with a temperature of " ++ show (round (temp - 273.15)) ++ "°C"

-- defines JSON parsing for the weather type
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

{- apiRequest
     helper function for performing the api request
     RETURNS: A DiscordHandler containing a ByteString representing the api response
     SIDE EFFECTS: performs an http request
-}
apiRequest :: String -> DiscordHandler S8.ByteString
apiRequest source = do
    request <- parseRequest source
    let request' = setRequestMethod (S8.pack "GET") request
    response <- httpBS request'
    return (getResponseBody response)
    
{- getWeather
     queries the openweathermap api for the local weather
     RETURNS: a DiscordHandler containing a MessageData containing the weather data represented as a Weather
     SIDE EFFECTS: performs an http request as well as decoding the resulting JSON
-}
getWeather :: DiscordHandler (Utils.MessageData (Maybe Weather))
getWeather = do
    json <- apiRequest "https://api.openweathermap.org/data/2.5/weather?q=Uppsala&appid=ce3a449055d96d97c82166fff5434393"
    let weather = Data.Aeson.decode $ BSL.fromStrict json

    Utils.toMessageData weather

-- extracts the icon value from the messagedata. then performs some string concatenation
weatherIcon :: (Utils.MessageData (Maybe Weather)) -> Text
weatherIcon Utils.Msg{Utils.value=(Just (Weather _ _ icon))} = pack ("http://openweathermap.org/img/wn/" ++ icon ++ "@2x.png") 

{- handleMessage message
     gets the weatherdata and the icon before using the standard messagehandling from utils
     SIDE EFFECTS: same as Utils.handleMessage, performs a rest call to the discord api
-}
handleMessage :: Message -> DiscordHandler ()
handleMessage m = do
    weather <- getWeather
    let icon = weatherIcon weather
    Utils.handleMessage m weather "Todays weather" icon
    

