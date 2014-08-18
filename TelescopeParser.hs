module TelescopeParser where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 as AB
import Data.Word
import Data.List
import Control.Monad
import Control.Applicative

data TelescopeTag = TelescopeTag B.ByteString deriving (Eq)
data TelescopeTree = TelescopeNode TelescopeTag [TelescopeTree] | TelescopeLeaf TelescopeTag deriving (Show, Eq)

instance Show TelescopeTag where
	show (TelescopeTag bs) = show bs

--instance Show TelescopeTree where
	--show (TelescopeTree tag t) = if t == [] then show tag else (show tag) ++ "(" ++ (concat $ intersperse "," $ map show t) ++ ")"

isAcceptableChar :: Char -> Bool
isAcceptableChar c = (isAlpha_ascii c) || (c == '.')

parseTag :: Parser TelescopeTag
parseTag = do
	tag <- AB.takeWhile (isAcceptableChar)
	return $ TelescopeTag tag

parseTree :: Parser TelescopeTree
parseTree = 
	(do
		e <- parseTag
		char '('
		t <- parseTree `sepBy` (char ',')
		char ')'
		return $ TelescopeNode e t
	) <|>
	(do
		e <- parseTag
		return $ TelescopeLeaf e
	)

parseStmt :: Parser [TelescopeTree]
parseStmt = do
	t <- parseTree `sepBy` (char ',')
	return  t
