{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Base
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C
import Data.Aeson (object, (.=), encode)
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


main :: IO ()
main = do
  manager <- newManager tlsManagerSettings

  -- Create the request
  let requestObject = object []
  initialRequest <- parseRequest "https://api.github.com/orgs/Microsoft/members?access_token=14b323f7634ef500b2886ef1979f2019f85dd12b"
  let hdr = [(Network.HTTP.Types.Header.hUserAgent, C.pack defaultUserAgent)]
  let request = initialRequest { method = "GET", requestBody = RequestBodyLBS "", requestHeaders = hdr}

  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  L.putStrLn $ responseBody response
