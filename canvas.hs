module Canvas where

{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple

data Course = Course { 
  course_id   :: Int 
, name :: String
, code :: String
} deriving (Show)

instance FromJSON Course where
    parseJSON (Object v) = do
        id <- v .: "id"
        name <- v .: "name"
        code <- v .: "course_code"
        return (Course { course_id = id ,name = name, code = code })
    parseJSON _ = mempty

--api request to json response
--lägg till felhantering
apiRequest :: String -> String -> IO S8.ByteString
apiRequest source token = do
    request <- parseRequest source
    let request' = setRequestMethod "GET"
                    $ setRequestHeader "Authorization" [S8.pack ("Bearer " ++ token)]
                    $ request
    response <- httpBS request'
    return (getResponseBody response)
    

getCourses :: String -> IO (Maybe [Course])
getCourses token = do
    json <- apiRequest "https://uppsala.instructure.com/api/v1/users/self/courses" token
    let 
        courses = Data.Aeson.decode $ BSL.fromStrict json :: Maybe [Course]
    return courses

--token: 14589~8OSCjplkTnDQZmlPnsyof6KoBPEjgkdwGWlcIp5iCLVVev85NMsdjuWk3lxse2OU
main :: IO ()
main = 
    do
    course <- getCourses "14589~8OSCjplkTnDQZmlPnsyof6KoBPEjgkdwGWlcIp5iCLVVev85NMsdjuWk3lxse2OU"
    print course
    

    {-
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
    -}