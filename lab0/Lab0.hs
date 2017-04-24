{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Network.HTTP.Conduit
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Network (withSocketsDo)

(email, name) = ("ulyanin1997@gmail.com", encodeUtf8 "Ульянин Д.А.") -- адрес почты и фамилия с инициалами

pascal :: Int -> Int -> Int
pascal k n
        | k == 0    = 1
        | n == k    = 1
        | k > n     = 0
        | otherwise = (pascal (k - 1) (n - 1)) + (pascal k (n - 1))


printIt :: Int -> C.ByteString
printIt n = C.pack $ show $ [pascal y x | x <- [0..n], y <- [0..x]]


main :: IO()
main =
  withSocketsDo $ do
  initReq <- parseUrl "http://91.239.142.110:13666/lab0"
  let req = urlEncodedBody [("email", email), ("name", name), ("content", printIt 20)] $ initReq { method = "POST" }
  response <- withManager $ httpLbs req
  L.putStr $ responseBody response
