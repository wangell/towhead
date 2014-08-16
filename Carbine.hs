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

dataDir = "~/code/carbine/dat/"
structDir = "~/code/carbine/struct/"
sqlFile = ".carbine.db"

-- Commands---------------------------
commands :: M.Map String ([String] -> IO())
commands = M.fromList $ 
	[("scan", scanCommand),
	("space", spaceCommand),
	("tag", tagCommand),
	("list", listCommand)]

listCommand :: [String] -> IO ()
listCommand args = do
	e <- getAllEntries
	let ts = map (\(x:y:z:[]) -> y) e
	mapM_ putStrLn (nub ts)

scanCommand :: [String] -> IO ()
scanCommand args = if args == [] then scanDataDir dataDir else scanDataDir (head args)

tagCommand :: [String] -> IO ()
tagCommand (tag:f:[]) = do
	mapM_ (\x -> addTag x f) (splitOn "," tag)

spaceCommand :: [String] -> IO ()
spaceCommand (args:[]) = do
	clearStructFolder
	filesByTag (splitOn "," args)
----------------------------------------

initializeCarbine = do
	y <- doesFileExist sqlFile
	case y of 
		True -> return ()
		False -> makeDB

createFolderStructure :: [String] -> IO ()
createFolderStructure xs = do
	let q = zipWith (++) (repeat structDir) xs
	mapM_ createDirectory q

makeDB :: IO ()
makeDB = do
	conn <- connectSqlite3 sqlFile
	run conn "CREATE TABLE files (md5string VARCHAR(256) NOT NULL UNIQUE, filename VARCHAR(1000))" []
	commit conn
	run conn "CREATE TABLE tags (md5string VARCHAR(256) NOT NULL, tag VARCHAR(256), PRIMARY KEY (md5string, tag))" []
	commit conn
	disconnect conn

recordFile :: String -> IO ()
recordFile x = do
	conn <- connectSqlite3 sqlFile
	filehash <- md5File x
	run conn "INSERT OR IGNORE INTO files VALUES (?, ?)" [toSql filehash, toSql x]
	commit conn
	disconnect conn

addTag :: String -> String -> IO ()
addTag t x = do
	conn <- connectSqlite3 sqlFile
	filehash <- md5File x
	run conn "INSERT OR IGNORE INTO tags VALUES (?, ?)" [toSql filehash, toSql t]
	commit conn
	disconnect conn

processEntry (x:y:z:[]) = do
	createSymbolicLink (z) (structDir ++ y ++ "/" ++ (takeFileName z))

scanDataDir :: FilePath -> IO ()
scanDataDir d = do
	f <- getDirectoryContents d
	mapM_ (recordFile . (d ++)) $ filter (isManaged) f

getEntries t = do
	conn <- connectSqlite3 sqlFile
 	q <- quickQuery' conn "SELECT t.*, f.filename from tags AS t JOIN files AS f ON t.md5string=f.md5string WHERE t.tag = ?" [toSql t]
	disconnect conn
 	return $ (map (\(x:y:z:[]) -> [fromSql x, fromSql y, fromSql z]) q :: [[String]])

getAllEntries = do
	conn <- connectSqlite3 sqlFile
 	q <- quickQuery' conn "SELECT t.*, f.filename from tags AS t JOIN files AS f ON t.md5string=f.md5string" []
	disconnect conn
 	return $ (map (\(x:y:z:[]) -> [fromSql x, fromSql y, fromSql z]) q :: [[String]])

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
isManaged s = (s /= ".carbine") && (not $ isDots s)

isDots :: String -> Bool
isDots s = (s == ".") || (s == "..")

main = do
	initializeCarbine
	args <- getArgs
	if args == [] then error "No command given."
	else
		let com = M.lookup (head args) commands in
			case com of
				Just action -> action (tail args)
				Nothing -> error "Invalid command"
