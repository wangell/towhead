import Data.List
import Numeric
import qualified Data.ByteString as B
import System.Directory
import System.Posix.Files
import System.FilePath.Posix
import Database.HDBC
import Database.HDBC.Sqlite3
import Crypto.Hash.MD5 as C
import Data.Convertible
import Data.List.Split
import System.Environment
import qualified Data.Map as M

structDir = "struct/"
sqlFile = ".towhead.db"

-- Commands---------------------------
commands :: M.Map String ([String] -> IO())
commands = M.fromList $ 
	[("scan", scanCommand),
	("space", spaceCommand),
	("tag", tagCommand),
	("list", listCommand),
	("init", initializeTowhead)]

listCommand :: [String] -> IO ()
listCommand args = do
	xs <- tagList
	mapM_ putStrLn xs

scanCommand :: [String] -> IO ()
scanCommand args = do
	aSpace <- doesFileExist sqlFile
	case aSpace of
		True -> 
			if args == [] 
			then scanDataDir "." 
			else scanDataDir (head args)
		False -> error "No active workspace."

tagCommand :: [String] -> IO ()
tagCommand (tags:files) = do
	canFiles <- mapM canonicalizePath files
	addTags (splitOn "," tags) canFiles

spaceCommand :: [String] -> IO ()
spaceCommand [] = do
	clearStructFolder
	defaultView
spaceCommand (args:[]) = do
	clearStructFolder
	let fullQ = splitOn "," args
	let uQ = filter ('.' `notElem`) fullQ
	let iQ = filter ('.' `elem`) fullQ
	filesByTag uQ
	filesByTagI $ map (splitOn ".") iQ

initializeTowhead :: [String] -> IO ()
initializeTowhead args = do
	y <- doesFileExist sqlFile
	case y of 
		True -> return ()
		False -> makeDB

----------------------------------------

createFolderStructure :: [String] -> IO ()
createFolderStructure xs = do
	let q = zipWith (++) (repeat structDir) xs
	mapM_ createDirectory q

tagList :: IO [String] 
tagList = do
	conn <- connectSqlite3 sqlFile
	q <- quickQuery' conn "SELECT DISTINCT tag FROM tags" []
	disconnect conn
	return $ map fromSql (concat q)

makeDB :: IO ()
makeDB = do
	conn <- connectSqlite3 sqlFile
	run conn "CREATE TABLE files (md5string VARCHAR(32) NOT NULL UNIQUE, filename VARCHAR(1000))" []
	run conn "CREATE TABLE tags (md5string VARCHAR(32) NOT NULL, tag VARCHAR(256), PRIMARY KEY (md5string, tag))" []
	commit conn
	disconnect conn

indexFiles :: [String] -> IO ()
indexFiles files = do
	conn <- connectSqlite3 sqlFile
	hashedFiles <- mapM md5File files
	let insSeq = zipWith (\x -> \y  -> [x,y]) (map toSql hashedFiles) (map toSql files)
	mapM_ (run conn "INSERT OR IGNORE INTO files VALUES (?, ?)") insSeq
	commit conn
	disconnect conn

addTags :: [String] -> [String] -> IO ()
addTags tags files = do
	conn <- connectSqlite3 sqlFile
	hashedFiles <- mapM md5File files
	let insSeq = sequence [(map toSql hashedFiles), (map toSql tags)]
	mapM_ (run conn "INSERT OR IGNORE INTO tags VALUES (?, ?)") insSeq
	commit conn
	disconnect conn

processEntry (x:y:z:[]) = do
	createLink (z) (structDir ++ y ++ "/" ++ (takeFileName z))
	--createSymbolicLink (z) (structDir ++ y ++ "/" ++ (takeFileName z))

processEntryDefault (x:y:[]) = do
	createLink (y) (structDir ++ (takeFileName y))
	--createSymbolicLink (y) (structDir ++ (takeFileName y))

scanDataDir :: FilePath -> IO ()
scanDataDir d = do
	can <- canonicalizePath d
	f <- getDirectoryContents can
	indexFiles $ map ((can ++ "/") ++) $ filter (isManaged) f

getEntriesIntersect t = do
	conn <- connectSqlite3 sqlFile
	let baseQuery = "SELECT t.md5string, f.filename FROM tags AS t JOIN files AS f ON t.md5string = f.md5string WHERE t.tag = ?"
	let compoundQuery = concat $ intersperse " INTERSECT " (replicate (length t) baseQuery)
 	q <- quickQuery' conn compoundQuery (map toSql t)
	disconnect conn
 	return $ (map (\(hash:fname:[]) -> [fromSql hash, (concat $ intersperse "." t), fromSql fname]) q :: [[String]])

getEntries t = do
	conn <- connectSqlite3 sqlFile
 	q <- quickQuery' conn "SELECT t.*, f.filename from tags AS t JOIN files AS f ON t.md5string=f.md5string WHERE t.tag = ?" [toSql t]
	disconnect conn
 	return $ (map (\(x:y:z:[]) -> [fromSql x, fromSql y, fromSql z]) q :: [[String]])

getAllEntries = do
	conn <- connectSqlite3 sqlFile
 	q <- quickQuery' conn "SELECT * from files" []
	disconnect conn
 	return $ (map (\(hash:fname:[]) -> [fromSql hash, fromSql fname]) q :: [[String]])

defaultView = do
	e <- getAllEntries
	mapM_ processEntryDefault e

filesByTagI t = do
	e <- mapM getEntriesIntersect t
	let ts = map (\(x:y:z:[]) -> y) (concat e)
	createFolderStructure (nub ts)
	mapM_ processEntry (concat e)

filesByTag t = do
	e <- mapM getEntries t
	let ts = map (\(x:y:z:[]) -> y) (concat e)
	createFolderStructure (nub ts)
	mapM_ processEntry (concat e)

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

md5File :: FilePath -> IO String
md5File f = do
	fc <- B.readFile (f)
	let q = concat $ map (\x -> showHex x "") $ B.unpack $ C.hash fc
	return q

isManaged :: FilePath -> Bool
isManaged s = (s /= ".towhead") && (not $ isDots s)

isDots :: String -> Bool
isDots s = (s == ".") || (s == "..")

main = do
	args <- getArgs
	if args == [] then error "No command given."
	else
		let com = M.lookup (head args) commands in
			case com of
				Just action -> action (tail args)
				Nothing -> error "Invalid command"
