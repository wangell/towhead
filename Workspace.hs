module Workspace where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import System.Posix.Files
import System.FilePath.Posix
import System.Directory
import TelescopeParser
import Storage

isManaged :: FilePath -> Bool
isManaged s = (s /= ".towhead.db") && (not $ isDots s) && (s /= ".dat")

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
	towDb <- findTowhead "."
	setCurrentDirectory (takeDirectory towDb)
	canPath <- canonicalizePath "."
	q <- getDirectoryContents canPath
	let r = filter (isManaged) q
	mapM_ (clearDirectory . ((canPath ++ "/") ++)) r

processEntry (x:y:z:[]) = do
	canPath <- canonicalizePath "."
	createLink (z) (canPath ++ "/" ++ y ++ "/" ++ (takeFileName z))

processEntryDefault (x:y:[]) = do
	canPath <- canonicalizePath "."
	createLink (y) (canPath ++ "/" ++ (takeFileName y))

createFolderStructure :: [String] -> IO ()
createFolderStructure xs = do
	canPath <- canonicalizePath "."
	let q = zipWith (++) (repeat (canPath ++ "/")) xs
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
createSpaceHelper (TelescopeNode (TelescopeTag t) children) baseTag = do
    filesByTagI [((BC.unpack t):baseTag)]
createSpaceHelper (TelescopeLeaf (TelescopeTag t)) baseTag = do
    filesByTagI [((BC.unpack t):baseTag)]
