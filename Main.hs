import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Directory
import System.Posix.Files
import System.FilePath.Posix
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
	("alias", aliasCommand),
	("connected", connectedCommand),
	("rm", removeCommand)]

removeCommand :: [String] -> IO ()
removeCommand [] = printUsage
removeCommand files = do
	canFiles <- mapM canonicalizePath files
	removeFromIndex canFiles

aliasCommand :: [String] -> IO ()
aliasCommand [] = printUsage
aliasCommand (alias:structure) = putStrLn ("Created '" ++ alias ++ "' alias.")

connectedCommand :: [String] -> IO ()
connectedCommand [] = printUsage
connectedCommand args = do
	xs <- connectedList (head args)
	putStrLn $ "# of tags connected to " ++ (head args) ++ " :"
	mapM_ (putStrLn . ("\t" ++ )) xs

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
	let fQ = filter ('+' `notElem`) $ filter ('.' `notElem`) fullQ
	let iQ = filter ('.' `elem`) fullQ
	let uQ = filter ('+' `elem`) fullQ
	filesByTag fQ
	filesByTagI $ map (splitOn ".") iQ
	filesByTagU $ map (splitOn "+") uQ

----------------------------------------

printUsage :: IO ()
printUsage = do
	putStrLn "Usage:"
	putStrLn "\ttowhead init <workspace-name> - initializes a workspace in the current directory"
	putStrLn "\ttowhead scan [directory] - creates index of the directory. If no directory is specified, uses the current directory."
	putStrLn "\ttowhead tag <comma delimited list of tags> <space delimited list of files> - tags each file with the list of tags."
	putStrLn "\ttowhead space - creates the default workspace view (union of all tags)"
	putStrLn "\ttowhead space [tag list delimited by commas] - creates workspace union of all tags, tags joined by a '.' will create an intersection"
	putStrLn "\ttowhead list [directory] - lists all tags in the current/specified workspace"
	putStrLn "\ttowhead connected [tag] - lists all tags that share the given tag pattern"
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
