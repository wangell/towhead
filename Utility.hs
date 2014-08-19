module Utility where

isDots :: String -> Bool
isDots s = (s == ".") || (s == "..")

isManaged :: FilePath -> Bool
isManaged s = (s /= ".towhead.db") && (not $ isDots s) && (s /= ".dat")
