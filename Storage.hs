module Storage where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List

sqlFile = ".towhead.db"

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
