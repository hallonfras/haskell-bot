{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Weather where
import Control.Monad

import           Data.Aeson
import           Data.Text
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple

import Discord
import Discord.Types
import qualified Discord.Requests as R

data Weather = Weather { 
    description :: String
  , temperature :: Float
} deriving (Show)


instance FromJSON Weather where
    parseJSON (Object obj) = do
        weatherObj <- obj .: "weather"
        let actualWeatherObj = Prelude.head weatherObj
        desc <- actualWeatherObj .: "description"
        dataObj <- obj .: "main"
        temp <- dataObj .: "temp"
        return (Weather { description = desc, temperature = temp })
    parseJSON _ = mempty

--api request to json response
--lägg till felhantering
apiRequest :: String -> IO S8.ByteString
apiRequest source = do
    request <- parseRequest source
    let request' = setRequestMethod (S8.pack "GET") $ request
    response <- httpBS request'
    return (getResponseBody response)
    

getWeather :: IO (Maybe Weather)
getWeather  = do
    json <- apiRequest "https://api.openweathermap.org/data/2.5/weather?q=Uppsala&appid=ce3a449055d96d97c82166fff5434393"
    let 
        weather = Data.Aeson.decode $ BSL.fromStrict json :: Maybe Weather
    return weather

weatherToString :: (Maybe Weather) -> String
weatherToString (Just (Weather desc temp)) = "The weather is" ++ desc
weatherToString Nothing = ""

handleMessage :: Message -> DiscordHandler ()
handleMessage m =  do
    weather <- getWeather
    let weatherString = weatherToString weather
    restCall (R.CreateMessage (messageChannel m) (pack (weatherToString weather)) )
    pure ()

--token: 14589~8OSCjplkTnDQZmlPnsyof6KoBPEjgkdwGWlcIp5iCLVVev85NMsdjuWk3lxse2OU
main :: IO ()
main = 
    do
    weather <- getWeather
    pure ()