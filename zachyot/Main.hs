module Main where
    
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
import Data.List(maximum)

task1 :: [t] -> [([t], [t])]
task1 a = task1' [] a [] where
    task1' acc [] x = ([], x) : acc
    task1' acc (y:ys) x = task1' ((x, y:ys) : acc) ys (x ++ [y])


task2 :: [Int] -> Bool
task2 a = 0 `elem` task2' a

task2' :: [Int] -> [Int]
task2' [] = []
task2' [a] = [a]
task2' a = concatMap (\(l, r) -> allOps (task2' l) (task2' r)) (task1 a) where
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]
    allOps l r = concatMap (\(x, y) -> [x * y, x - y, x + y]) (cartProd l r)
    addOne [] x = [x]
    addOne l x = allOps l [x]



data Arythm t
    = Leaf t
    | Node Op (Arythm t) (Arythm t)
    deriving (Eq)
    -- deriving (Eq, Show)

instance Show t => Show (Arythm t) where
    show (Leaf t) = show t
    show (Node op l r) = addBrackets (show l ++ " " ++ show op ++ " " ++  show r) where
        addBrackets x = "(" ++ x ++ ")"


data Op = Plus | Minus | Product
    deriving (Eq, Show)


applyOp Plus    = (+)
applyOp Minus   = (-)
applyOp Product = (*)

execArythm :: Arythm Int -> Int
execArythm (Leaf x) = x
execArythm (Node op l r) = applyOp op (execArythm l) (execArythm r)

task4 :: [Int] -> [Arythm Int]
task4 a = filter ((==0) . execArythm ) (task4' a)


task4' :: [Int] -> [Arythm Int]
task4' [] = []
task4' [a] = [Leaf a]
task4' a = concatMap (\(l, r) -> allOps (task4' l) (task4' r)) (task1 a) where
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]
    allOps l r = concatMap (uncurry allOps') (cartProd l r)
    allOps' a1 a2 = [ Node Plus    a1 a2
                    , Node Minus   a1 a2
                    , Node Product a1 a2]
    addOne [] x = [Leaf x]
    addOne l x = allOps l [Leaf x]


task5 :: [Int] -> Int
task5 = maximum . map execArythm . task4'

main :: IO()
main = do
    print 2
