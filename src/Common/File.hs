-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Common.File( 
                  -- * File names
                    takeBaseName, takeFileName, notext, joinPath, joinPaths, takeExtension, takeDirectory
                  , undelimPaths, splitDirectories
                  , isPathSep, isPathDelimiter
                  , searchPathSeparator
                  , isAbsolute
                  , commonPathPrefix
                  ) where

import Data.List        ( intersperse, isPrefixOf )
import qualified System.FilePath as FilePath

import Test.QuickCheck

{--------------------------------------------------------------------------
  File paths
--------------------------------------------------------------------------}

-- | Remove the extension and directory part
takeBaseName :: FilePath -> FilePath
takeBaseName
  = FilePath.takeBaseName

-- | Get the file extension
takeExtension :: FilePath -> FilePath
takeExtension
  = FilePath.takeExtension

-- | Return the directory prefix
takeDirectory :: FilePath -> FilePath
takeDirectory
  = FilePath.takeDirectory

-- | Remove the directory prefix
takeFileName :: FilePath -> FilePath
takeFileName
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
joinPath
  = FilePath.combine

-- | Join a list of paths into one path
joinPaths :: [FilePath] -> FilePath
joinPaths
  = foldl FilePath.combine ""

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
  = joinPaths $ map fst
              $ takeWhile (\(c,d) -> c == d)
              $ zip (splitDirectories s1) (splitDirectories s2)
