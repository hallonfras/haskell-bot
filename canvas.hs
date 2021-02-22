import Data.Aeson (Value)
import Network.HTTP.Simple

main :: IO ()
main = do
    request <- parseRequest "POST http://httpbin.org/post"
    response <- httpJSON request