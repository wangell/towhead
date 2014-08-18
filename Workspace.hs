module Workspace where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import System.Posix.Files
import System.FilePath.Posix
import System.Directory
import TelescopeParser
import Storage

structDir = "struct/"

isManaged :: FilePath -> Bool
isManaged s = (s /= ".towhead") && (not $ isDots s)

isDots :: String -> Bool
isDots s = (s == ".") || (s == "..")

clearDirectory :: FilePath -> IO ()
clearDirectory f = do
    isDir <- doesDirectoryExist f
    case isDir of
        True -> do
            q <- getDirectoryContents f
            let r = filter (isManaged) q
            case (r == []) of
                True -> removeDirectory f
                False -> do
                    mapM_ (clearDirectory . ((f ++ "/") ++)) r
                    removeDirectory f
        False -> do
            removeFile f

clearStructFolder :: IO ()
clearStructFolder = do
    q <- getDirectoryContents structDir
    let r = filter (isManaged) q
    mapM_ (clearDirectory . ((structDir ++ "/") ++)) r

processEntry (x:y:z:[]) = do
    createLink (z) (structDir ++ y ++ "/" ++ (takeFileName z))
    --createSymbolicLink (z) (structDir ++ y ++ "/" ++ (takeFileName z))

processEntryDefault (x:y:[]) = do
    createLink (y) (structDir ++ (takeFileName y))
    --createSymbolicLink (y) (structDir ++ (takeFileName y))

createFolderStructure :: [String] -> IO ()
createFolderStructure xs = do
    let q = zipWith (++) (repeat structDir) xs
    mapM_ createDirectory q

filesByTagI :: [[String]] -> IO ()
filesByTagI t = do
	e <- mapM getEntriesIntersect t
	let ts = map (\(x:y:z:[]) -> y) (concat e)
	createFolderStructure (nub ts)
	mapM_ processEntry (concat e)

filesByTagU :: [[String]] -> IO ()
filesByTagU t = do
	e <- mapM getEntriesUnion t
	let ts = map (\(x:y:z:[]) -> y) (concat e)
	createFolderStructure (nub ts)
	mapM_ processEntry (concat e)

filesByTag t = do
	e <- mapM getEntries t
	let ts = map (\(x:y:z:[]) -> y) (concat e)
	createFolderStructure (nub ts)
	mapM_ processEntry (concat e)

createSpace :: [TelescopeTree] -> [String] -> IO ()
createSpace ((TelescopeLeaf (TelescopeTag t)):[]) baseTag = do
    clearStructFolder
    filesByTagI [((BC.unpack t):baseTag)]

createSpaceHelper :: TelescopeTree -> [String] -> IO ()
createSpaceHelper (TelescopeLeaf (TelescopeTag t)) baseTag = do
    filesByTagI [((BC.unpack t):baseTag)]
