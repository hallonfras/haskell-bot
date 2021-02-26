{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

--module Canvas where

import Data.Data
import           Data.Aeson
import           Data.String.Builder
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple
import qualified Language.Haskell.Interpreter as Hint

data Course = Course { 
  course_id   :: String
, name :: String
, code :: String
} deriving (Data,Typeable,Show)

instance FromJSON Course where
    parseJSON (Object v) = do
        id <- v .: "id"
        name <- v .: "name"
        code <- v .: "course_code"
        return (Course { course_id = id ,name = name, code = code })
    parseJSON _ = mempty

--builds string with data of object
buildString :: (Data a,Show a) => a -> IO String
buildString obj = do
    let fields = constrFields (toConstr obj) -- gets list of datatype fields
    buildString' fields obj "" -- calls builder loop
--loop that builds the full string that shall be returned by the discord bot
buildString' :: Show t => [String] -> t -> String -> IO String
buildString' fields obj str = do
    if null fields
    then do return str
    else do
        value' <- Hint.runInterpreter $ Hint.loadModules ["DataT.hs"] >> Hint.setTopLevelModules ["DataT"]>>Hint.setImports["Prelude"] >> 
                                Hint.interpret ("DataT." ++ head fields ++ " " ++ (show obj)) (Hint.as :: String) -- runs function
        let value = (\(Right v) -> v) value' -- Fetches the result from Either datatype
            s = str ++ "\n" ++ (head fields ++ " : " ++ value)
        buildString' (tail fields) obj s


--api request to json response
--lägg till felhantering

apiRequest :: String -> [Char] -> IO (Response S8.ByteString)
apiRequest source token = do
    request <- parseRequest source
    let request' = setRequestMethod "GET"
                    $ setRequestHeader "Authorization" [S8.pack ("Bearer " ++ token)]
                    $ request
    httpBS request'

--getCourses :: String -> IO (Maybe [Course])
getCourses :: String -> IO (Maybe [Course])
getCourses token = do
    response <- apiRequest "https://uppsala.instructure.com/api/v1/users/self/courses" token
    let 
        json = getResponseBody response
        courses = Data.Aeson.decode $ BSL.fromStrict json :: Maybe [Course]
    return courses

apiSuccess :: Maybe a -> Bool 
apiSuccess Nothing = False 
apiSuccess _ = True

sample = Course "0" "name" "code"

fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a 

--token: 14589~8OSCjplkTnDQZmlPnsyof6KoBPEjgkdwGWlcIp5iCLVVev85NMsdjuWk3lxse2OU
test :: IO ()
test = 
    do
    --courses <- getCourses "14589~8OSCjplkTnDQZmlPnsyof6KoBPEjgkdwGWlcIp5iCLVVev85NMsdjuWk3lxse2OU"
    --print courses
    --str <- buildString (head (fromMaybe courses))
    --print str
    str <-buildString (Course "302" "Algebra 1" "ALG1BLABLA")
    print str

    {-
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
    -}