{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Base as HTTPBase
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import GHC.Generics
import Control.Exception as X
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Concurrent.Async

--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   -- manager <- newManager defaultManagerSettings
--
--   -- Create the request
--   -- let requestObject = object ["name" .= "Michael", "age" .= 30]
--   -- initialRequest <- parseRequest "https://api.github.com/orgs/Microsoft/members?access_token=14b323f7634ef500b2886ef1979f2019f85dd12b"
--   initialRequest <- parseRequest "http://github.com"
--   let hdr = [(Network.HTTP.Types.Header.hUserAgent, C.pack defaultUserAgent)]
--   let request = initialRequest { method = "HEAD",  requestHeaders = hdr}
--
--   response <- httpLbs request manager
--   putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
--   print $ responseBody response

data Member = Member {
      id          :: Int
    , memberLogin :: T.Text
    , memberUrl   :: T.Text
    , memberType  :: T.Text
    } deriving (Generic, Show)


instance FromJSON Member where
    parseJSON = withObject "Member" $ \v -> Member
        <$> v .: "id"
        <*> v .: "login"
        <*> v .: "url"
        <*> v .: "type"

instance ToJSON Member

data User = User {
      userLogin :: T.Text
    , blog  :: T.Text
    } deriving (Generic, Show)

nullUser = User { userLogin = "", blog = "" }

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "login"
        <*> v .: "blog"

instance ToJSON User

type GitHubResponse = Network.HTTP.Client.Response L.ByteString

-- statusExceptionHandler ::  SomeException -> IO GitHubResponse
-- statusExceptionHandler e = putStrLn ("status:\n") >> return L.empty


urlForPage :: Int -> String
urlForPage n = "https://api.github.com/orgs/Microsoft/public_members?access_token=14b323f7634ef500b2886ef1979f2019f85dd12b&page=" ++ show n

addTokenToURL :: String -> String
addTokenToURL = (++ "?access_token=14b323f7634ef500b2886ef1979f2019f85dd12b")


delayPage :: IO ()
delayPage = Control.Concurrent.threadDelay 500000

delayBetweenChunks :: IO ()
delayBetweenChunks = do
    putStrLn "long delay"
    Control.Concurrent.threadDelay $ 50 * 1000000
    return ()


getResponseByUrl :: Manager -> String -> IO GitHubResponse
getResponseByUrl manager url = do
    putStrLn $ "getting url: " ++ url

    -- Create the request
    initialRequest <- parseRequest url
    let hdr = [(Network.HTTP.Types.Header.hUserAgent, C.pack defaultUserAgent)]
    let request = initialRequest { method         = "GET"
                                 , requestBody    = RequestBodyLBS ""
                                 , requestHeaders = hdr}

    httpLbs request manager


getResponseByUrlAsync :: Int -> Manager -> String -> IO (Async GitHubResponse)
getResponseByUrlAsync timeout manager url = do

    -- Create the request
    -- manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest url
    let hdr = [(Network.HTTP.Types.Header.hUserAgent, C.pack defaultUserAgent)]
    let request = initialRequest { method         = "GET"
                                 , requestBody    = RequestBodyLBS ""
                                 , requestHeaders = hdr
                                 , responseTimeout = responseTimeoutMicro timeout }

    async $ httpLbs request manager


getResponseByPageNumber :: Manager -> Int -> IO GitHubResponse
getResponseByPageNumber manager pageNumber = getResponseByUrl manager (urlForPage pageNumber)


response2Members :: GitHubResponse -> [Member]
response2Members response = fromMaybe [] (decode $ responseBody response :: Maybe [Member])


response2user :: GitHubResponse -> User
response2user response = fromMaybe nullUser (decode $ responseBody response :: Maybe User)


getMembers :: Manager -> [Int] -> IO [Member]
getMembers manager pageNumbers = concatMap response2Members <$> responses where
    responses :: IO [GitHubResponse]
    responses = mapM (getResponseByPageNumber manager) pageNumbers


getUserInfo :: Manager -> Member -> IO User
getUserInfo manager member = do
    response <- getResponseByUrl manager (T.unpack $ memberUrl member)
    return $ response2user response


hasBlog :: Manager -> Member -> IO Bool
hasBlog manager member = do
    putStrLn $ "getting info for " ++ show (memberUrl member)
    user <- getUserInfo manager member
    let hasNoContent = T.null $ blog user
    unless hasNoContent
        (print user)
    putStrLn $ "complete info for " ++ T.unpack (memberLogin member)
    return $ not hasNoContent

lab3' :: [Int] -> IO [T.Text]
lab3' pageNumbers = do
    manager <- newManager tlsManagerSettings
    memberResponses <- mapM (getResponseByUrl manager . urlForPage) pageNumbers
    let members = concatMap response2Members memberResponses
    putStrLn $ "members = " ++ init (show $ take 10 members) ++ ".."
    userResponses <- mapM (getResponseByUrl manager . addTokenToURL . T.unpack . memberUrl) members
    let users = map response2user userResponses
    putStrLn $ "users = " ++ init (show $ take 10 userResponses) ++ ".."
    return $ map userLogin $ filter (not . T.null . blog)  users

lab3 :: IO [T.Text]
lab3 = lab3' [1..100]


lab3Async :: IO [T.Text]
lab3Async = do
    manager <- newTlsManager
    let timeout = 1000 * 1000 * 1000  -- 1000 seconds
    let getResponseByUrlAsync' = getResponseByUrlAsync (10 ^ 9) manager
    memberResponsesAsync <- mapM (getResponseByUrlAsync' . urlForPage) [1..112]
    memberResponses <- mapM wait memberResponsesAsync
    let members = concatMap response2Members memberResponses
    print $ length members
    putStrLn $ "members = " ++ init (show $ take 10 members) ++ ".."
    userResponsesAsync <- mapM (getResponseByUrlAsync' . addTokenToURL . T.unpack . memberUrl) members
    userResponses <- mapM wait userResponsesAsync
    let users = map response2user userResponses
    putStrLn $ "users = " ++ init (show $ take 10 userResponses) ++ ".."
    return $ map userLogin $ filter (not . T.null . blog)  users


main :: IO ()
main = do
  users <- lab3Async
  print users
  -- Create the request
  -- let requestObject = object []
  -- initialRequest <- parseRequest "https://api.github.com/orgs/Microsoft/members?access_token=14b323f7634ef500b2886ef1979f2019f85dd12b"
  -- let hdr = [(Network.HTTP.Types.Header.hUserAgent, C.pack defaultUserAgent)]
  -- let request = initialRequest { method = "GET", requestBody = RequestBodyLBS "", requestHeaders = hdr}
  --
  -- response <- httpLbs request manager
  -- putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  -- L.putStrLn $ responseBody response
  -- print (decode $ responseBody response :: Maybe [Member])
