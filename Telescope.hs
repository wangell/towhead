module Telescope where

import Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 as AB
import Data.Word
import Control.Monad
import Control.Applicative

data TelescopeTag = TelescopeTag ByteString deriving (Show)
data TelescopeExpr = TelescopeExpr [TelescopeTag] deriving (Show)
data TelescopeTree = TelescopeTree TelescopeExpr [TelescopeTree] deriving (Show)

isAcceptableChar :: Char -> Bool
isAcceptableChar c = (isAlpha_ascii c) || (c == '.')

parseTag :: Parser TelescopeTag
parseTag = do
	tag <- AB.takeWhile (isAcceptableChar)
	return $ TelescopeTag tag

parseExpr :: Parser TelescopeExpr
parseExpr = do
	elements <- parseTag `sepBy` (char ',')
	return $ TelescopeExpr elements

parseTree :: Parser TelescopeTree
parseTree = 
	(do
		e <- parseTag
		char '('
		t <- parseTree `sepBy` (char ',')
		char ')'
		return $ TelescopeTree (TelescopeExpr [e]) t
	) <|>
	(do
		e <- parseTag
		return $ TelescopeTree (TelescopeExpr [e]) []
	)

parseStmt :: Parser [TelescopeTree]
parseStmt = do
	t <- parseTree `sepBy` (char ',')
	return  t
