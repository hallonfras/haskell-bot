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

import qualified Utils


data Weather = Weather { 
    description :: String
  , temperature :: Float
  , icon        :: String
} deriving (Show)

instance Utils.Stringable Weather where
    stringIt (Weather desc temp _) = "The weather in Uppsala is " ++ desc ++ " with a temperature of " ++ show (temp - 273.15)

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

--api request to json response
--lägg till felhantering
apiRequest :: String -> DiscordHandler S8.ByteString
apiRequest source = do
    request <- parseRequest source
    let request' = setRequestMethod (S8.pack "GET") request
    response <- httpBS request'
    return (getResponseBody response)
    
    
getWeather :: DiscordHandler (Utils.MessageData (Maybe Weather))
getWeather  = do
    let api = "https://api.openweathermap.org/data/2.5/weather?q=Uppsala&appid=ce3a449055d96d97c82166fff5434393"
    json <- apiRequest api
    let weather = Data.Aeson.decode $ BSL.fromStrict json
    Utils.toMessageData weather


weatherIcon :: Utils.MessageData (Maybe Weather) -> Text
weatherIcon Utils.Msg{Utils.value=(Just (Weather _ _ icon))} = pack ("http://openweathermap.org/img/wn/" ++ icon ++ "@2x.png") 

handleMessage :: Message -> DiscordHandler ()
handleMessage m = do
    weather <- getWeather
    let icon = weatherIcon weather
    Utils.handleMessage m weather "Todays weather" icon
    

