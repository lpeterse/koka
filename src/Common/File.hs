-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Common.File( 
                  -- * File names
                    basename, notdir, notext, joinPath, joinPaths, extname, dirname
                  , undelimPaths, splitDirectories
                  , isPathSep, isPathDelimiter
                  , searchPathSeparator
                  , findMaximalPrefix
                  , isAbsolute
                  , commonPathPrefix
                  ) where

import Data.List        ( intersperse, isPrefixOf )
import qualified System.FilePath as FilePath

{--------------------------------------------------------------------------
  File paths
--------------------------------------------------------------------------}

-- | Remove the extension and directory part
basename :: FilePath -> FilePath
basename
  = FilePath.takeBaseName

-- | Get the file extension
extname :: FilePath -> FilePath
extname
  = FilePath.takeExtension

-- | Return the directory prefix
dirname :: FilePath -> FilePath
dirname
  = FilePath.takeDirectory

-- | Remove the directory prefix
notdir :: FilePath -> FilePath
notdir
  = FilePath.takeFileName

notext :: FilePath -> FilePath
notext
  = FilePath.dropExtensions

-- | Split a (semi-)colon separated list of directories into a directory list
undelimPaths :: String -> [FilePath]
undelimPaths
  = FilePath.splitSearchPath

-- FIXME: current code base cannot deal with empty result list!
splitDirectories :: FilePath -> [FilePath]
splitDirectories x
  = let y = FilePath.splitDirectories x
    in  if null y then [""] else y

joinPath :: FilePath -> FilePath -> FilePath
joinPath p1 p2
  = FilePath.combine p1 p2

-- | Join a list of paths into one path
joinPaths :: [FilePath] -> FilePath
joinPaths
  = foldl joinPath ""

-- | Is this a file separator.
isPathSep :: Char -> Bool
isPathSep c
  = FilePath.isPathSeparator c

-- | Is this a path delimiter? (@;@ (and @:@ too on unix)
isPathDelimiter :: Char -> Bool
isPathDelimiter c
  = FilePath.isSearchPathSeparator c

searchPathSeparator :: Char
searchPathSeparator
  = FilePath.searchPathSeparator

isAbsolute :: FilePath -> Bool
isAbsolute
  = FilePath.isAbsolute

commonPathPrefix :: FilePath -> FilePath -> FilePath
commonPathPrefix s1 s2
  = joinPaths $ map fst $ takeWhile (\(c,d) -> c == d) $ zip (splitDirectories s1) (splitDirectories s2)

-- | Find a maximal prefix given a string and list of prefixes. Returns the prefix and its length.
findMaximalPrefix :: [String] -> String -> Maybe (Int,String)
findMaximalPrefix xs s
  = findMaximal (\x -> if isPrefixOf s x then Just (length x) else Nothing) xs

findMaximal :: (a -> Maybe Int) -> [a] -> Maybe (Int,a)
findMaximal f xs
  = normalize Nothing xs
  where
    normalize res []     = res
    normalize res (x:xs) = case (f x) of
                        Just n  -> case res of
                                     Just (m,y)  | m >= n -> normalize res xs
                                     _           -> normalize (Just (n,x)) xs
                        Nothing -> normalize res xs