{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad              ()
import           Control.Monad.State
import           Data.ByteString.Internal   (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.String                (IsString)
import           Data.Char
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Network                    (withSocketsDo)
import           Network.HTTP.Conduit
import           System.Directory
import           System.Environment         ()
import           System.IO
import           System.Random

-- почтовый адрес
email :: Data.ByteString.Internal.ByteString
email = ""

data JSON
    = JSNULL
    | JSObject  [(String, JSON)]
    | JSArray   [JSON]
    | JSInt     !Int
    | JSString  !String
    | JSBool    !Bool

-- добавим сответствующие классы типов для JSON


instance Show JSON where
  show json = case json of
      JSNULL        -> "null"
      JSInt n       -> show n
      JSString s    -> show s
      JSBool b      -> show b
      JSObject []   -> "{}"
      JSObject list -> "{" ++
                        (init . init) (foldl (\acc t -> acc ++ showPair t ++ ",") "" list) ++
                         "}" where
                             showPair (key, jsValue) = show key ++ ": " ++ show jsValue
      JSArray list  -> show list


instance Read JSON where
  readsPrec _ = parseObject


-- object
--      {}
--      { members }
-- members
--      pair
--      pair , members
-- pair
--      string : value
-- array
--      []
--      [ elements ]
-- elements
--      value
--      value , elements
-- value
--      string
--      number
--      object
--      array
--      true
--      false
--      null

-- NOT IMPLEMENTED YET
parseObject :: ReadS JSON
parseObject s
            | s == "{}" = [(JSObject [], "")]
            | otherwise = case lex s of
                [(h, t)] -> case h of
                    "{"     -> let [(members, rest)] = parseMembers t
                                in [(JSObject members, rest)]
                    _       -> error $ "parsing Object expected '{'', found '" ++ h ++ "'"
                _        -> error "unexpected lex behaviour"


parseMembers :: ReadS [(String, JSON)]
parseMembers s = let [(c, rest)]    = lex s
                 in case c of
                    "}" -> [([], rest)]
                    "," -> let [(pair, rest')]   = parsePair rest
                               [(pairs, rest'')] = parseMembers rest'
                            in [(pair : pairs, rest'')]
                    _   -> let [(pair, rest')]   = parsePair s
                               [(pairs, rest'')] = parseMembers rest'
                            in [(pair : pairs, rest'')]


parsePair :: ReadS (String, JSON)
parsePair s = let [(key, rest)]         = lex s
                  [(splitted, rest')]   = lex rest
              in case splitted of
                  ":" -> let [(value, rest'')] = parseValue rest'
                         in [((read key, value), rest'')]
                  _   -> error $ "parsing Pair unuxpected spliting symbol '" ++ splitted ++ "';" ++ rest'


parseValue :: ReadS JSON
parseValue s = let [(word, rest)] = lex s
               in case head word of
                   '{'                              -> parseObject s
                   '['                              -> parseArray s
                   '"'                              -> [(JSString $ read word, rest)]
                   c
                      | isDigit c                   -> [(JSInt (read word :: Int), rest)]
                      | map toUpper word == "TRUE"  -> [(JSBool True, rest)]
                      | map toUpper word == "FALSE" -> [(JSBool False, rest)]
                      | map toUpper word == "NULL"  -> [(JSNULL, rest)]
                      | otherwise                   ->
                            error $ "parsing Value unexpected value type, found '" ++ word ++ "'; " ++ rest


parseArray :: ReadS JSON
parseArray s = let [(brace, rest)] = lex s
               in  case brace of
                       "[" -> parseElements rest
                       _   -> error $ "parsing Array expeced [ found'" ++ brace ++ "'"

parseElements :: ReadS JSON
parseElements s = let [(splitted, rest)] = lex s
                  in  case splitted of
                        "]" -> [(JSArray [], rest)]
                        "," -> let [(value, rest')]     = parseValue rest
                                   [(JSArray values, rest'')] = parseElements rest'
                               in  [(JSArray $ value : values, rest'')]
                        _   -> let [(value, rest')]     = parseValue s
                                   [(JSArray values, rest'')] = parseElements rest'
                               in  [(JSArray $ value : values, rest'')]


lab3 :: Num t => JSON -> t
lab3 (JSObject list) = 0

stringify :: Data.String.IsString t => JSON -> t
stringify (JSObject list) = "{}"

-- вариант с монадой IO
generateIO :: IO JSON
generateIO = do
  num <- randomRIO (1, 2) :: IO Int
  let json = case num of
               1 -> JSArray [];
               2 -> JSObject [("io", JSNULL)]
               _ -> error "unexpected number found"
  return json

-- чистый вариант с генератором, заключённым в состояние
-- мы храним в состоянии генератор, каждый раз используя
-- его, возвращаем в состояние новый

type GeneratorState = State StdGen

generate' :: GeneratorState JSON
generate' = do
  gen <- get
  let (num, newGen) = randomR (1, 2) gen :: (Int, StdGen)
  let json = case num of
               1 -> JSNULL;
               2 -> JSArray [JSObject [("pure", JSNULL)]]
               _ -> error "unexpected random number found"
  put newGen
  return json

generate :: JSON
generate = evalState generate' (mkStdGen 0)

main :: IO()
main = withSocketsDo $ do
  dir <- getCurrentDirectory
  initReq <- parseUrl "http://localhost:13666/lab2"
  -- initReq <- parseUrl "http://91.239.142.110:13666/lab2"
  handle <- openFile (dir ++ "/Lab2.hs") ReadMode
  hSetEncoding handle utf8_bom
  content <- hGetContents handle
  let req = urlEncodedBody [("email", email), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
  response <- withManager $ httpLbs req
  hClose handle
  L.putStrLn $ responseBody response
