module Storage where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import Crypto.Hash.MD5 as C
import qualified Data.ByteString as B
import Numeric
import System.Directory

sqlFile = ".towhead.db"

findTowhead :: FilePath -> IO String
findTowhead dir = do
	canPath <- canonicalizePath dir
	if canPath == "/home"
	then error "Couldn't find Towhead db"
	else do
		f <- doesFileExist (canPath ++ "/" ++ sqlFile)
		case f of
			True -> return (canPath ++ "/" ++ sqlFile)
			False -> findTowhead (canPath ++ "/..")

md5File :: FilePath -> IO String
md5File f = do
    fc <- B.readFile (f)
    let q = concat $ map (\x -> showHex x "") $ B.unpack $ C.hash fc
    return q

connectedList :: String -> IO [String]
connectedList tag = do
	--towDb <- findTowhead "."
	--conn <- connectSqlite3 (towDb ++ "/" ++ sqlFile)
	conn <- connectSqlite3 sqlFile
	q <- quickQuery' conn "SELECT parent.tag, COUNT(parent.tag) FROM tags LEFT JOIN tags AS parent ON tags.md5string = parent.md5string WHERE tags.tag = ? AND parent.tag != ? GROUP BY parent.tag ORDER BY COUNT(parent.tag) DESC" [toSql tag, toSql tag]
	disconnect conn
	return $ map (\(tag:count:[]) -> ((fromSql tag) ++ " - " ++ (fromSql count))) q

tagList :: FilePath -> IO [String]
tagList dir = do
	conn <- connectSqlite3 (dir ++ "/" ++ sqlFile)
	q <- quickQuery' conn "SELECT tag, COUNT(tag) FROM tags GROUP BY tag ORDER BY COUNT(tag) DESC" []
	disconnect conn
	return $ map (\(tag:count:[]) -> ((fromSql tag) ++ " - " ++ (fromSql count))) q

removeFromIndex :: [String] -> IO ()
removeFromIndex files = do
	towDb <- findTowhead "."
	conn <- connectSqlite3 towDb
	hashedFiles <- mapM md5File files
	mapM_ (run conn "DELETE FROM files WHERE md5string = ?") (map (\x -> [toSql x]) hashedFiles)
	mapM_ (run conn "DELETE FROM tags WHERE md5string = ?") (map (\x -> [toSql x]) hashedFiles)
	commit conn
	disconnect conn

indexFiles :: [String] -> IO ()
indexFiles files = do
	conn <- connectSqlite3 sqlFile
	hashedFiles <- mapM md5File files
	let insSeq = zipWith (curry (\(x,y) -> [x,y]))  (map toSql hashedFiles) (map toSql files)
	mapM_ (run conn "INSERT OR IGNORE INTO files VALUES (?, ?)") insSeq
	commit conn
	disconnect conn

addTags :: [String] -> [String] -> IO ()
addTags tags files = do
	towDb <- findTowhead "."
	conn <- connectSqlite3 towDb
	hashedFiles <- mapM md5File files
	let insSeq = sequence [(map toSql hashedFiles), (map toSql tags)]
	mapM_ (run conn "INSERT OR IGNORE INTO tags VALUES (?, ?)") insSeq
	commit conn
	disconnect conn

makeDB :: IO ()
makeDB = do
    conn <- connectSqlite3 sqlFile
    run conn "CREATE TABLE files (md5string VARCHAR(32) NOT NULL UNIQUE, filename VARCHAR(1000))" []
    run conn "CREATE TABLE tags (md5string VARCHAR(32) NOT NULL, tag VARCHAR(256), PRIMARY KEY (md5string, tag))" []
    commit conn
    disconnect conn

getEntriesIntersect t = do
    conn <- connectSqlite3 sqlFile
    let baseQuery = "SELECT t.md5string, f.filename FROM tags AS t JOIN files AS f ON t.md5string = f.md5string WHERE t.tag = ?"
    let compoundQuery = concat $ intersperse " INTERSECT " (replicate (length t) baseQuery)
    q <- quickQuery' conn compoundQuery (map toSql t)
    disconnect conn
    return $ (map (\(hash:fname:[]) -> [fromSql hash, (concat $ intersperse "." t), fromSql fname]) q :: [[String]])

getEntriesUnion t = do
    conn <- connectSqlite3 sqlFile
    let baseQuery = "SELECT t.md5string, f.filename FROM tags AS t JOIN files AS f ON t.md5string = f.md5string WHERE t.tag = ?"
    let compoundQuery = concat $ intersperse " UNION " (replicate (length t) baseQuery)
    q <- quickQuery' conn compoundQuery (map toSql t)
    disconnect conn
    return $ (map (\(hash:fname:[]) -> [fromSql hash, (concat $ intersperse "+" t), fromSql fname]) q :: [[String]])

getEntries t = do
    conn <- connectSqlite3 sqlFile
    q <- quickQuery' conn "SELECT t.*, f.filename from tags AS t JOIN files AS f ON t.md5string=f.md5string WHERE t.tag = ?" [toSql t]
    disconnect conn
    return $ (map (\(x:y:z:[]) -> [fromSql x, fromSql y, fromSql z]) q :: [[String]])

getAllEntries :: IO [[String]]
getAllEntries = do
    conn <- connectSqlite3 sqlFile
    q <- quickQuery' conn "SELECT * from files" []
    disconnect conn
    return $ (map (\(hash:fname:[]) -> [fromSql hash, fromSql fname]) q :: [[String]])
