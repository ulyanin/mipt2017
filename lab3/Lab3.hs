{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding
import Network.HTTP.Conduit
import qualified Data.Text as T
import Data.List.Split
import Data.List(any, intercalate, intersperse)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, ($//), (&|), (&//), (&/), (>=>), attribute)
import Network (withSocketsDo)
import Control.Concurrent

-- почтовый адрес
email = "ulyanin1997@gmail.com"

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- withSocketsDo $ simpleHttp u
     return $ fromDocument $ parseLBS page


urlForPage :: Int -> String
urlForPage n = "http://github.com/orgs/Microsoft/people?page=" ++ show n


delayPage :: IO ()
delayPage = Control.Concurrent.threadDelay 500000

delayBetweenChunks :: IO ()
delayBetweenChunks = do
    putStrLn "long delay"
    Control.Concurrent.threadDelay $ 50 * 1000000
    return ()


getCursorByPageNumber :: Int -> IO Cursor
getCursorByPageNumber n = do
    putStrLn $ "getting " ++ show (urlForPage n)
    delayPage
    cursorFor $ urlForPage n


getUserList :: Cursor -> [T.Text]
getUserList cursor = cursor $// element "a" >=> attributeIs "class" "css-truncate-target f4" &| T.concat . attribute "href"


getUsers :: [Int] -> IO [String]
getUsers pageNumbers = concat <$> usersInChunks where
    chunks :: [[Int]]
    chunks = Data.List.Split.chunksOf 25 pageNumbers


    usersInChunks :: IO [[String]]
    usersInChunks = foldr applyFilter (return []) chunks

    applyFilter :: [Int] -> IO [[String]] -> IO [[String]]
    applyFilter pageNumbers ioxs = do
        xs <- ioxs
        unless (null xs)
            delayBetweenChunks
        cursors <- mapM getCursorByPageNumber pageNumbers
        let pages = map T.unpack $ concatMap getUserList cursors
        return $ pages : xs


blogOnMSDN :: String -> IO Bool
blogOnMSDN username = do
    putStrLn $ "getting info for" ++ username
    cursor <- cursorFor $ "https://github.com" ++ username
    let hrefs = cursor $// element "ul"
                >=> attributeIs "class" "vcard-details border-top border-gray-light py-3"
                &// element "a" &| T.concat . attribute "href"
    Control.Concurrent.threadDelay 500000
    let contains = Data.List.any (\text -> T.pack "social.msdn.microsoft.com" `T.isInfixOf` text) hrefs
    unless (not contains) (print username)
    putStrLn $ "complete info for" ++ username
    return contains


getUsersWithBlog :: [String] -> IO [String]
getUsersWithBlog userNames = concat <$> usersInChunks where
    -- split userList to chunks of size 40
    chunks :: [[String]]
    chunks = Data.List.Split.chunksOf 25 userNames

    -- without sleep
    -- usersInChunks :: IO [[String]]
    -- usersInChunks = mapM (filterM blogOnMSDN) chunks

    -- filter users in every chunk
    usersInChunks :: IO [[String]]
    usersInChunks = foldr applyFilter (return []) chunks

    -- applying filter in chunks via sleep
    applyFilter :: [String] -> IO [[String]] -> IO [[String]]
    applyFilter x ioxs = do
        xs <- ioxs
        unless (null xs)
            delayBetweenChunks
        filtered <- filterM blogOnMSDN x
        return $ filtered : xs


lab3 :: IO [T.Text]
lab3 = do
    users <- getUsers [1..100]
    filtered <- getUsersWithBlog users
    return $ map T.pack filtered
    -- return $ cursor $// element "a" >=> attributeIs "class" "css-truncate-target f4" &| T.concat . attribute "href"
    -- return users3
    -- return $ cursor $// element "ul" >=> attributeIs "class" "right-menu" &// element "a" >=> child &| T.concat . content


main :: IO()
main = withSocketsDo $ do
    nodes <- lab3
    dir <- getCurrentDirectory
    initReq <- parseUrl "http://91.239.142.110:13666/lab3"
    handle <- openFile (dir ++ "/Lab3.hs") ReadMode
    hSetEncoding handle utf8_bom
    content <- hGetContents handle
    let req = urlEncodedBody [("email", email), ("result", encodeUtf8 $ T.concat $ nodes), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
    response <- withManager $ httpLbs req
    hClose handle
    L.putStrLn $ responseBody response