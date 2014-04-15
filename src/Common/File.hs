-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Common.File( 
                  -- * File names
                    takeBaseName
                  , takeFileName
                  , dropExtensions
                  , joinPath
                  , joinPaths
                  , takeExtension
                  , takeDirectory
                  , splitSearchPath
                  , splitDirectories
                  , isPathSep
                  , searchPathSeparator
                  , commonPathPrefix
                  , normalise
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

dropExtensions :: FilePath -> FilePath
dropExtensions
  = FilePath.dropExtensions

-- | Split a (semi-)colon separated list of directories into a directory list
splitSearchPath :: String -> [FilePath]
splitSearchPath
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

searchPathSeparator :: Char
searchPathSeparator
  = FilePath.searchPathSeparator

commonPathPrefix :: FilePath -> FilePath -> FilePath
commonPathPrefix s1 s2
  = joinPaths $ map fst
              $ takeWhile (\(c,d) -> c == d)
              $ zip (splitDirectories s1) (splitDirectories s2)

normalise :: FilePath -> FilePath
normalise
  = FilePath.normalise
