{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric   #-}

module Canvas where
import Data.Data
import Data.List
import           Data.Aeson
import Data.Aeson.Types
import           Data.String.Builder
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BSL
import           Network.HTTP.Simple
import qualified Language.Haskell.Interpreter as Hint
import Discord
import Discord.Types



data Folder = Folder {
    folder_id :: String,
    folder_name :: String, 
    parent_id :: Maybe String, --root folder has no parent_id
    children :: [Folder],
    files :: [File]
}
data File = File {
    file_id :: String,
    file_name :: String,
    file_url :: String
} deriving (Show,Eq)

data Course = Course { 
  course_id   :: String
, course_name :: String
, course_code :: String
} deriving (Data,Typeable,Show)

data Assignment = Assignment {
    assignment_id :: String,
    assignment_name :: String,
    points_possible :: String,
    assignment_url :: String,
    created_at :: String
} deriving (Data,Typeable,Show)

data MessageData a = Msg {
    value :: a,
    error :: Bool,
    errorMsg :: String
}

instance Eq Folder where
    (==) f1 f2 = folder_id f1 == folder_id f2 && 
                                folder_name f1 == folder_name f2 && 
                                parent_id f1 == parent_id f2 && 
                                children f1 == children f2 && 
                                files f1 == files f2

instance Show Folder where
    show f = " {fid: " ++ show (folder_id f) ++ " fname: " ++ show (folder_name f) ++ " fpid: " ++ show (parent_id f) ++ " children: " ++ show (children f) ++ " files: " ++ show (files f) ++ "} "


instance FromJSON Folder where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        parent <- v .:? "parent_folder_id" -- .:? takes into account for Null and nonexisting keys
        return $ Folder (show (id ::Int)) name (maybeToString (parent :: Maybe Int)) [] [] --new folder does not have children nor files
    parseJSON _ = mempty

maybeToString :: Show a => Maybe a -> Maybe String
maybeToString (Just i) = Just (show i)
maybeToString Nothing = Nothing

instance FromJSON Course where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        code <- v .: "course_code"
        return (Course {course_id = (show (id :: Int)),course_name= name,course_code= code})
    parseJSON _ = mempty

instance FromJSON Assignment where
    parseJSON (Object v) = do
        id <- v .: "id" 
        name <- v .: "name"
        points <- v .: "points_possible"
        url <- v .: "html_url"
        created_at <- v .: "created_at"
        return $ Assignment (show (id :: Int)) name (show (points :: Double)) url created_at
    parseJSON _ = mempty

--builds a string from a data record
dataToString :: (Data a,Show a) => a -> DiscordHandler String
dataToString obj = do
    let fields = constrFields (toConstr obj) -- gets list of datatype fields
    buildString' fields obj "" -- calls builder loop
--loop that builds the full string that shall be returned by the discord bot
buildString' :: Show t => [String] -> t -> String -> DiscordHandler String
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

apiRequest :: String -> [Char] -> DiscordHandler (S8.ByteString)
apiRequest source token = do
    request <- parseRequest source
    let request' = setRequestMethod "GET"
                    $ setRequestHeader "Authorization" [S8.pack ("Bearer " ++ token)]
                    $ request
    response <- httpBS request'
    return (getResponseBody response)

getCourses :: String -> DiscordHandler (MessageData [Course])
getCourses token = do
    json <- apiRequest "https://uppsala.instructure.com/api/v1/users/self/courses" token
    jsonToMessageData json

-- https://uppsala.instructure.com/api/v1/courses/26396
getAssignments :: String -> String -> DiscordHandler (MessageData [Assignment])
getAssignments token courseid = do
    json <- apiRequest ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/assignments") token
    jsonToMessageData json

apiRequest' :: String -> [Char] -> IO (S8.ByteString)
apiRequest' source token = do
    request <- parseRequest source
    let request' = setRequestMethod "GET"
                    $ setRequestHeader "Authorization" [S8.pack ("Bearer " ++ token)]
                    $ request
    response <- httpBS request'
    return (getResponseBody response)
getFolders :: String -> String -> IO ([Folder])
getFolders token courseid = do
    json <- apiRequest' ("https://uppsala.instructure.com/api/v1/courses/" ++ courseid ++ "/folders") token
    let value = Data.Aeson.decode $ BSL.fromStrict json
    return (fromMaybe value)

--rename the local functions so it becomes more clear
sortFolders :: [Folder] -> Folder
sortFolders folderList = findRoot folderList folderList
    where
        findRoot (f:fs) folders = -- searches for root folder
            if parent_id f == Nothing then buildFolder (delete f folders) f -- when root folder is found, calls aux function and removes root from golders
            else findRoot fs folders
            where
                --builds folder structure from root to bottom
                buildFolder [] root = root
                buildFolder (x:xs) root@(Folder r_id r_name r_pid children files)
                    | fromMaybe (parent_id x) == r_id = buildFolder xs (Folder r_id r_name r_pid ((buildFolder (delete f folders) x):children) files) --if f has parent_id equal to root iq then add (f with its children) to children 
                    | otherwise = buildFolder xs root



jsonToMessageData :: FromJSON a => S8.ByteString -> DiscordHandler (MessageData [a])
jsonToMessageData json = do
    let 
        value = Data.Aeson.decode $ BSL.fromStrict json
    if apiFail value
    then do return (Msg [] True "Failed to retrieve data")
    else do return (Msg (fromMaybe value) False "")


--builds a string of a data record list
dataListToString :: (Data a, Show a) => [a] -> DiscordHandler String
dataListToString objs = dataListToString' objs ""

--accumulator function to build string of the object list
dataListToString' :: (Data a, Show a) => [a] -> String -> DiscordHandler String
dataListToString' [] str = return str
dataListToString' (o:os) str = do
    ostr <- dataToString o
    dataListToString' os (str ++ ostr ++ "\n")

apiFail :: Maybe a -> Bool 
apiFail Nothing = True
apiFail _ = False

fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a
