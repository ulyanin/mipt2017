module Sem9 where

import System.IO
import Control.Exception

data Tree tLeaf tNode =
      LeafNode tLeaf
    | InternalNode tNode [Tree tLeaf tNode]


foldTree :: (t -> tleaf -> t) -> (t -> tnode -> t) -> t -> Tree tleaf tnode -> t
foldTree fLeaf fNode = fold' where
    fold' acc tree = case tree of
        LeafNode leafInfo              ->
            fLeaf acc leafInfo
        InternalNode nodeInfo subTrees ->
            foldl fold' (fNode acc nodeInfo) subTrees


mapTree :: (tLeaf -> tLeaf') -> (tNode -> tNode') -> Tree tLeaf tNode -> Tree tLeaf' tNode'
mapTree fLeaf fNode = mapTree' where
    mapTree' (LeafNode leafInfo)              = LeafNode $ fLeaf leafInfo
    mapTree' (InternalNode nodeInfo subTrees) =
        InternalNode (fNode nodeInfo) (map mapTree' subTrees)

data FileInfo = FileInfo {fileName :: String,
                          fileSize :: Int}

data DirectoryInfo = DirectoryInfo {dirName :: String,
                                    dirSize :: Int}

data FileSyztemItem = Tree FileInfo DirectoryInfo

fromFile :: tLeaf -> Tree tLeaf tNode
fromFile = LeafNode

fromDir :: tNode -> [Tree tLeaf tNode] -> Tree tLeaf tNode
fromDir = InternalNode

readme :: Tree FileInfo tNode
readme = fromFile FileInfo {fileName="readme.txt", fileSize=1}

config :: Tree FileInfo tNode
config = fromFile FileInfo {fileName="config.json", fileSize=2}

build :: Tree FileInfo tNode
build  = fromFile FileInfo {fileName="build.sh", fileSize=3}

src :: Tree FileInfo DirectoryInfo
src = fromDir DirectoryInfo {dirName="src", dirSize=10} [readme, config, build]

bin :: Tree FileInfo DirectoryInfo
bin = fromDir DirectoryInfo {dirName="bin", dirSize=10} []

root :: Tree FileInfo DirectoryInfo
root = fromDir DirectoryInfo {dirName="root", dirSize=5} [src, bin]

totalSize :: Tree FileInfo DirectoryInfo -> Int
totalSize =
    foldTree fFile fDir 0 where
        fFile acc file = acc + fileSize file
        fDir acc dir = acc + dirSize dir


largestFile :: Tree FileInfo DirectoryInfo -> Maybe Int
largestFile =
    foldTree fFile fDir Nothing where
        fFile acc file = maxMaybe acc (Just $ fileSize file)
        fDir acc _ = acc
        maxMaybe Nothing b = b
        maxMaybe a Nothing = a
        maxMaybe (Just a) (Just b) = Just $ max a b


-- getFileSize :: FilePath -> IO Integer
-- getFileSize x = do
--     handle <- openFile x ReadMode
--     size <- hFileSize handle
--     hClose handle
--     return size

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler
                   $ withFile path ReadMode (\h -> do
                       size <- hFileSize h
                       return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing
