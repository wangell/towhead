import Data.List
import Numeric
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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
import TelescopeParser
import Workspace
import Storage

data FileEntry = FileEntry { hash :: String
							,filePath :: String
							,tag :: String } deriving (Show)

-- Commands---------------------------
commands :: M.Map String ([String] -> IO())
commands = M.fromList $ 
	[("scan", scanCommand),
	("space", spaceCommand),
	("tag", tagCommand),
	("list", listCommand),
	("init", initializeTowhead),
	("alias", aliasCommand)]

aliasCommand :: [String] -> IO ()
aliasCommand [] = printUsage
aliasCommand (alias:structure) = putStrLn ("Created '" ++ alias ++ "' alias.")

listCommand :: [String] -> IO ()
listCommand args = do
	if args == [] then do
		xs <- tagList "./"
		mapM_ putStrLn xs
	else do
		canPath <- canonicalizePath (head args)
		xs <- tagList canPath
		mapM_ putStrLn xs

scanCommand :: [String] -> IO ()
scanCommand args = do
	aSpace <- doesFileExist sqlFile
	case aSpace of
		True -> 
			if args == [] 
			then scanDataDir "." 
			else scanDataDir (head args)
		False -> error "No active workspace.  To create a workspace in this directory execute 'towhead init <workspace-name>'"

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

tagList :: FilePath -> IO [String] 
tagList dir = do
	conn <- connectSqlite3 (dir ++ "/" ++ sqlFile)
	q <- quickQuery' conn "SELECT tag, COUNT(tag) FROM tags GROUP BY tag ORDER BY COUNT(tag) DESC" []
	disconnect conn
	return $ map (\(tag:count:[]) -> ((fromSql tag) ++ " - " ++ (fromSql count))) q

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
	conn <- connectSqlite3 sqlFile
	hashedFiles <- mapM md5File files
	let insSeq = sequence [(map toSql hashedFiles), (map toSql tags)]
	mapM_ (run conn "INSERT OR IGNORE INTO tags VALUES (?, ?)") insSeq
	commit conn
	disconnect conn

scanDataDir :: FilePath -> IO ()
scanDataDir d = do
	can <- canonicalizePath d
	f <- getDirectoryContents can
	indexFiles $ map ((can ++ "/") ++) $ filter (isManaged) f

defaultView :: IO ()
defaultView = do
	e <- getAllEntries
	mapM_ processEntryDefault e

md5File :: FilePath -> IO String
md5File f = do
	fc <- B.readFile (f)
	let q = concat $ map (\x -> showHex x "") $ B.unpack $ C.hash fc
	return q

printUsage :: IO ()
printUsage = do
	putStrLn "Usage:"
	putStrLn "\ttowhead init <workspace-name> - initializes a workspace in the current directory"
	putStrLn "\ttowhead scan [directory] - creates index of the directory. If no directory is specified, uses the current directory."
	putStrLn "\ttowhead tag <comma delimited list of tags> <space delimited list of files> - tags each file with the list of tags."
	putStrLn "\ttowhead space - creates the default workspace view (union of all tags)"
	putStrLn "\ttowhead space [tag list delimited by commas] - creates workspace union of all tags, tags joined by a '.' will create an intersection"
	putStrLn "\ttowhead list [directory] - lists all tags in the current/specified workspace"
	putStrLn ""

main = do
	args <- getArgs
	if args == [] then do
		putStrLn "No command given."
		printUsage
	else let com = M.lookup (head args) commands in
			case com of
				Just action -> action (tail args)
				Nothing -> do
					putStrLn "Invalid command"
					printUsage
