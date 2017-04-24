{-# LANGUAGE OverloadedStrings #-}

module Lab1 (main, fTailor, tailor, tailorA, fSolve, iter, newton, dichotomy) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Network                    (withSocketsDo)
import           Network.HTTP.Conduit
import           System.Directory
import           System.Environment
import           System.IO

-- почтовый адрес
email = "ulyanin1997@gmail.com"
-- общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = (Double, Integer)


-- variant 18
fTailor :: Floating f => f -> f
fTailor x = (1.0 + x * x) / 2.0 * atan x - x / 2.0 -- функция, которую раскладываем

delta :: Double
delta = 1e-10
(n, a, b) = (20, 0.1, 0.6) -- интервал

tailor :: Double -> Result
tailor x = (last listTerms, fromIntegral $ length listTerms)
    where
        realRes = fTailor x
        listTerms = Data.List.unfoldr (\(k, sumRes) ->
                                    if abs (realRes - sumRes) < delta then
                                        Nothing
                                    else
                                        Just (sumRes, (k + 1, sumRes + nthTerm k))) (1, 0.0)
        nthTerm :: Integer -> Double
        nthTerm k = fromIntegral ((-1) ^ (k + 1)) * (x ^ fromIntegral (2 * k + 1)) / fromIntegral (4 * k ^ 2 - 1)

tailorA :: Double -> Result
tailorA x = (last listTerms, fromIntegral $ length listTerms)
    where
        realRes = fTailor x
        listTerms = Data.List.unfoldr (\(k, currentTerm, sumRes) ->
                                    if abs (realRes - sumRes) < delta then
                                        Nothing
                                    else
                                        Just (sumRes, (k + 1, nextTerm currentTerm (k + 1), sumRes + currentTerm))) (1, nthTerm 1, 0.0)
        nthTerm :: Integer -> Double
        nthTerm k = fromIntegral ((-1) ^ (k + 1)) * (x ^ fromIntegral (2 * k + 1)) / fromIntegral (4 * k ^ 2 - 1)

        nextTerm :: Double -> Integer -> Double
        nextTerm fPrev k = fPrev * (-1) * x ^ 2 * fromInteger (4 * (k - 1) ^ 2 - 1) / fromIntegral (4 * k ^ 2 - 1)

printTailor = mapM_ putStrLn $
	map
		(\ x ->
			let ((firstRes, firstCou), (secondRes, secondCou)) = (tailor x, tailorA x)
			in
                show x ++ "\t" ++ show firstRes ++ "\t" ++ show firstCou ++ "\t" ++ 
                show secondRes ++ "\t" ++ show secondCou ++ "\t" ++ show (fTailor x))
		[a, a + (b - a) / n .. b]


-- *** Вторая часть

fun18 :: Double -> Double
fun18 x = x + sqrt x + x ** (1.0 / 3.0) - 2.5

-- d/dx fun18
fun18' :: Double -> Double
fun18' x = 1.0 + 1.0 / 2 / sqrt x + (1.0 / 3.0) / (x ** (2.0 / 3.0))

fun19 :: Double -> Double
fun19 x = x - 1.0 / (3 + sin (3.6 * x))

fun19' :: Double -> Double
fun19' x = 1 + (3.6 * cos (3.6 * x)) / (sin (3.6 * x) + 3) ^ 2

fun20 :: Double -> Double
fun20 x = 0.1 * x ^ 2 - x * log x

fun20' :: Double -> Double
fun20' x = 0.2 * x  - log x - 1

functions :: [(Double -> Double, Double -> Double, Double, Double)]
functions = [(fun18, fun18', 0.4, 1), (fun19, fun19', 0, 0.85), (fun20, fun20', 1, 2)]

-- fSolve = \x -> x -- функция, решение которой ищем
fSolve = fun19

iter :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Result
iter f f' a b = iter' ((a + b) / 2) 0
    where
        k = 1 / f' b
        iter' x n
                | abs (k * f x) < delta = (x, n)
                | otherwise             = iter' (x - k * f x) (n + 1)

newton :: (Double -> Double) ->(Double -> Double) -> Double -> Double -> Result
newton f f' a b = newton' ((b + a) / 2) 0
    where
        newton' x n
                | abs curDelta < delta = (x, n)
                | otherwise             = newton' (x - curDelta) (n + 1)
                where curDelta = f x / f' x

dichotomy :: (Double -> Double) ->(Double -> Double) -> Double -> Double -> Result
dichotomy f f' = dichotomy' 0
    where
        dichotomy' n a b
                        | abs (b - a) < delta   = (m, n)
                        | f b * f m < 0         = dichotomy' (n + 1) m b
                        | otherwise             = dichotomy' (n + 1) a m
                        where m = (a + b) / 2

printSolve =
	mapM_ putStrLn
            [
                "\n[" ++ show a ++ "; "++ show b ++ "]\n" ++
                    unwords
                        (map (\f -> show $ f fSolve fSolve' a b) [iter, newton, dichotomy])
                | (fSolve, fSolve', a, b) <- functions
            ]

main :: IO()
main = withSocketsDo $ do
    printTailor
    dir <- getCurrentDirectory
    -- initReq <- parseUrl "http://91.239.142.110:13666/lab1"
    initReq <- parseUrl "http://192.168.80.59:13666"
    handle <- openFile (dir ++ "/Lab1.hs") ReadMode
    hSetEncoding handle utf8_bom
    content <- hGetContents handle
    let req = urlEncodedBody [("email", email), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
    response <- withManager $ httpLbs req
    hClose handle
    L.putStrLn $ responseBody response
