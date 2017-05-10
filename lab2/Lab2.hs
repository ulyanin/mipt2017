{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad              ()
import           Control.Monad.State
import           Data.ByteString.Internal   (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.String                (IsString, fromString)
import           Data.Char
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Network                    (withSocketsDo)
import           Network.HTTP.Conduit
import           System.Directory
import           System.Environment         ()
import           System.IO
import           System.Random
import           Control.Arrow

-- почтовый адрес
email :: Data.ByteString.Internal.ByteString
email = "ulyanin1997@gmail.com"

data JSON
    = JSNULL
    | JSObject  [(String, JSON)]
    | JSArray   [JSON]
    | JSInt     !Int
    | JSString  !String
    | JSBool    !Bool


data Token
    = OpenBrace
    | CloseBrace
    | OpenBracket
    | CloseBracket
    | Colon
    | Comma
    | TokenNull
    | TokenString String
    | TokenNumber Int
    | TokenBool Bool
    deriving (Show, Eq)

type ReadToken t = [Token] -> (t, [Token])

-- добавим сответствующие классы типов для JSON


instance Show JSON where
  show json = case json of
      JSNULL        -> "null"
      JSInt n       -> show n
      JSString s    -> show s
      JSBool b      -> map Data.Char.toLower $ show b
      JSObject []   -> "{}"
      JSObject list -> "{" ++
                        init (foldl (\acc t -> acc ++ showPair t ++ ",") "" list) ++
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



tokenizeString :: String -> [Token]
tokenizeString s = case s of
    []                         -> []
    '{' : s'                   -> OpenBrace : tokenizeString s'
    '}' : s'                   -> CloseBrace : tokenizeString s'
    '[' : s'                   -> OpenBracket : tokenizeString s'
    ']' : s'                   -> CloseBracket : tokenizeString s'
    ':' : s'                   -> Colon : tokenizeString s'
    ',' : s'                   -> Comma : tokenizeString s'
    _
        | map Data.Char.toLower (take 5 s) == "false"
                               -> TokenBool False : tokenizeString (drop 5 s)
    _
        | map Data.Char.toLower (take 4 s) == "true"
                               -> TokenBool True : tokenizeString (drop 4 s)
    _
        | map Data.Char.toLower (take 4 s) == "null"
                               -> TokenNull : tokenizeString (drop 4 s)
    -- 'n' : 'u' : 'l' : 'l' : s' -> TokenNull : tokenizeString s'
    -- 'f' : 'a' : 'l' : 's' : 'e' : s' -> TokenBool False : tokenizeString s'
    -- 't' : 'r' : 'u' : 'e' : s' -> TokenBool True : tokenizeString s'
    '"' : s'                   -> let [(tokenStr, rest)] = lex s
                                   in TokenString (read tokenStr) : tokenizeString rest
    w : s'
        | Data.Char.isSpace w -> tokenizeString s'
        | Data.Char.isDigit w -> let [(tokenNumber, rest)] = lex s
                                  in TokenNumber (read tokenNumber :: Int) : tokenizeString rest


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

parseObjectT :: ReadToken JSON
parseObjectT tokens = case tokens of
    [TokenNull]             -> (JSNULL, [])
    [OpenBrace, CloseBrace] -> (JSObject [], [])
    OpenBrace : tokens'     -> let (members, rest) = parseMembersT tokens'
                                in (JSObject members, rest)
    _                       -> error $ "unexpected toleken" ++ show tokens

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


parseMembersT :: ReadToken [(String, JSON)]
parseMembersT tokens = case tokens of
    CloseBrace : rest -> ([], rest)
    Comma      : rest -> let (pair, rest')   = parsePairT rest
                             (pairs, rest'') = parseMembersT rest'
                          in (pair : pairs, rest'')
    _                 -> let (pair, rest')   = parsePairT tokens
                             (pairs, rest'') = parseMembersT rest'
                          in (pair : pairs, rest'')


parsePair :: ReadS (String, JSON)
parsePair s = let [(key, rest)]         = lex s
                  [(splitted, rest')]   = lex rest
              in case splitted of
                  ":" -> let [(value, rest'')] = parseValue rest'
                         in [((read key, value), rest'')]
                  _   -> error $ "parsing Pair unuxpected spliting symbol '" ++ splitted ++ "';" ++ rest'

parsePairT :: ReadToken (String, JSON)
parsePairT tokens = case tokens of
    TokenString key : Colon : rest -> let (value, rest') = parseValueT rest
                                       in ((key, value), rest')
    _                              -> error $ "parsing Pair Token '" ++ show tokens

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


parseValueT :: ReadToken JSON
parseValueT allTokens@(token:tokens) = case token of
    OpenBrace       -> parseObjectT allTokens
    OpenBracket     -> parseArrayT allTokens
    TokenString str -> (JSString str, tokens)
    TokenBool bool  -> (JSBool bool, tokens)
    TokenNumber num -> (JSInt num, tokens)
    TokenNull       -> (JSNULL, tokens)
    _               -> error $ "parsing Value unexpected token " ++ show token ++ "---" ++ show tokens


parseArray :: ReadS JSON
parseArray s = let [(brace, rest)] = lex s
               in  case brace of
                       "[" -> parseElements rest
                       _   -> error $ "parsing Array expeced [ found'" ++ brace ++ "'"

parseArrayT :: ReadToken JSON
parseArrayT (token:tokens) = case token of
    OpenBracket -> parseElementsT tokens
    _           -> error $ "parsing Array expected [ found '" ++ show token ++ "'"

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


parseElementsT :: ReadToken JSON
parseElementsT allTokens@(token:tokens) = case token of
    CloseBracket -> (JSArray [], tokens)
    Comma        -> let (value, tokens')           = parseValueT tokens
                        (JSArray values, tokens'') = parseElementsT tokens'
                     in (JSArray $ value : values, tokens'')
    _            -> let (value, tokens')           = parseValueT allTokens
                        (JSArray values, tokens'') = parseElementsT tokens'
                     in (JSArray $ value : values, tokens'')


-- Вариант 18
lab3 :: JSON -> JSON
lab3 = lab3' 0 where
    lab3' d jsObject = case jsObject of
                            JSInt   v     -> JSInt (v + d)
                            JSArray array -> JSArray $ map (lab3' d) array
                            JSObject objs -> JSObject $ map (Control.Arrow.second (lab3' (d + 1))) objs
                            _             -> jsObject


stringify :: Data.String.IsString t => JSON -> t
stringify = Data.String.fromString . show

-- вариант с монадой IO
generateIO :: IO JSON
generateIO = generateObjectIO


generateObjectIO :: IO JSON
generateObjectIO = do
    size <- randomRIO (0, 5) :: IO Int
    keyLength <- randomRIO (1, 10) :: IO Int
    values <- generateArrayIO size
    keys <- generateStringsIO keyLength size
    return $ JSObject $ zip keys values


generateValueIO :: IO JSON
generateValueIO = do
  num <- randomRIO (1, 6) :: IO Int
  case num of
        1 -> return JSNULL
        2 -> return $ JSBool False
        3 -> return $ JSBool True
        4 -> do
            value <- randomRIO (-5, 5) :: IO Int
            return $ JSInt value
        5 -> do
            size <- randomRIO (1, 5) :: IO Int
            array <- generateArrayIO size
            return $ JSArray array
        _ -> generateObjectIO


generateArrayFuncIO :: IO t -> Int -> IO [t]
generateArrayFuncIO _ 0 = return []
generateArrayFuncIO g n = do
    value <- g
    rest <- generateArrayFuncIO g (n - 1)
    return $ value : rest


generateArrayIO :: Int -> IO [JSON]
generateArrayIO = generateArrayFuncIO generateValueIO

generateStringIO :: Int -> IO String
generateStringIO = generateArrayFuncIO (randomRIO ('A', 'Z') :: IO Data.Char.Char)

generateStringsIO :: Int -> Int -> IO [String]
generateStringsIO sSize = generateArrayFuncIO $ generateStringIO sSize

-- чистый вариант с генератором, заключённым в состояние
-- мы храним в состоянии генератор, каждый раз используя
-- его, возвращаем в состояние новый

-- не получилось :(

-- type GeneratorState = State StdGen
--
-- generateValue :: GeneratorState JSON
-- generateValue = do
--     gen <- get
--     let (num, newGen) = randomR (1, 5) gen :: (Int, StdGen)
--     let (jsonValue, newGen') = case num of
--                  1 -> (JSNULL, gen);
--                  2 -> (JSBool False, gen)
--                  3 -> (JSBool True, gen)
--                  _ -> let (num', gen') = randomR (-100, 100) newGen :: (Int, StdGen)
--                       in (JSInt num', gen')
--     put newGen'
--     return jsonValue
--
-- generate' :: GeneratorState JSON
-- generate' = do
--   gen <- get
--   let (num, newGen) = randomR (2, 2) gen :: (Int, StdGen)
--   let (json, newGen') = case num of
--                1 -> runState generateValue newGen
--                _ -> let (size, gen')   = randomR (0, 10) newGen :: (Int, StdGen)
--                         (array, gen'') = runState (generateArray size) gen'
--                      in (JSArray array, gen'')
--             --    _ -> error "unexpected random number found"
--   put newGen'
--   return json
--
--
-- generateArray :: Int -> GeneratorState [JSON]
-- generateArray 0 = return []
-- generateArray n = do
--     gen <- get
--     let (value, newGen) = randomR (2, 2) gen :: (Int, StdGen)
--     -- let (value, newGen) = runState generateValue gen
--     -- put newGen
--     let (values, newGen') = runState (generateArray $ n - 1) newGen
--     put newGen'
--     return (JSInt value : values)
--
--
-- generate :: Int -> JSON
-- generate seed = JSArray $ evalState (generateArray 10) (mkStdGen seed)



main :: IO()
main = withSocketsDo $ do
    js <- generateIO
    print js
    print $ lab3 js
    let js' = read "{\"a\" : 0, \"b\" : {\"c\" : 0, \"d\" : [0, 1, 0], \"e\" : {\"f\" : 0}}}" :: JSON
    print js'
    print $ lab3 js'
    dir <- getCurrentDirectory
    initReq <- parseUrl "http://91.239.142.110:13666/lab2"
    handle <- openFile (dir ++ "/Lab2.hs") ReadMode
    hSetEncoding handle utf8_bom
    content <- hGetContents handle
    let req = urlEncodedBody [("email", email), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
    response <- withManager $ httpLbs req
    hClose handle
    L.putStrLn $ responseBody response
