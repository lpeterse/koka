-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Internal errors and assertions.
-}
-----------------------------------------------------------------------------
module Common.File( 
                  -- * System
                    getEnvPaths, getEnvVar
                  , searchPaths, searchPathsEx
                  , runSystem, runSystemRaw
                  , getProgramPath, getInstallDir

                  -- * File names
                  , basename, notdir, notext, joinPath, joinPaths, extname, dirname
                  , undelimPaths, splitDirectories
                  , isPathSep, isPathDelimiter
                  , searchPathSeparator
                  , findMaximalPrefix
                  , isAbsolute
                  , commonPathPrefix

                  -- * Files
                  , FileTime, fileTime0, maxFileTime, maxFileTimes
                  , fileTimeCompare, getFileTime
                  , getFileTimeOrCurrent, getCurrentTime
                  , copyTextFile, copyTextIfNewer, copyTextIfNewerWith, copyTextFileWith
                  , copyBinaryFile, copyBinaryIfNewer
                  ) where

import Data.List        ( intersperse, isPrefixOf )
import Data.Char        ( toLower, isSpace )
import qualified Platform.Runtime as B ( copyBinaryFile )
import Common.Failure   ( raiseIO, catchIO )
 
import System.Cmd       ( system )
import System.Exit      ( ExitCode(..) )
import System.Environment ( getEnvironment, getProgName )
import System.Directory ( doesFileExist, doesDirectoryExist
                        , copyFile
                        , getCurrentDirectory, getDirectoryContents
                        , createDirectoryIfMissing, canonicalizePath )
import qualified System.FilePath    as FilePath

import qualified Platform.Console as C (getProgramPath)
import Lib.Trace
import Platform.Filetime




{--------------------------------------------------------------------------
  File names
--------------------------------------------------------------------------}

-- | Remove the extension and directory part
basename :: FilePath -> String
basename  = FilePath.takeBaseName

-- | Get the file extension
extname :: FilePath -> FilePath
extname fname
  = let (pre,post) = span (/='.') (reverse (notdir fname))
    in if null post
        then ""
        else ("." ++ reverse pre) 

-- | Return the directory prefix (including last separator if present)
dirname :: FilePath -> FilePath
dirname fname
  = joinPaths (init (splitDirectories fname))

-- | Remove the directory prefix
notdir :: FilePath -> FilePath
notdir fname
  = last (splitDirectories fname)


notext :: FilePath -> FilePath
notext fname
  = reverse (drop (length (extname fname)) (reverse fname))  

-- | Split a (semi-)colon separated list of directories into a directory list
undelimPaths :: String -> [FilePath]
undelimPaths xs
  = filter (not . null) (normalize [] "" xs)
  where
    -- initial spaces
    normalize ps "" (c:cs)  | isSpace c
      = normalize ps "" cs
    -- directory on windows
    normalize ps "" (c:':':cs)    
      = normalize ps (':':c:[]) cs
    -- normal
    normalize ps p xs
      = case xs of
          []     -> if (null p)
                     then reverse ps
                     else reverse (reverse p:ps)
          (c:cs) | isPathDelimiter c -> normalize (reverse p:ps) "" cs
                 | otherwise         -> normalize ps (c:p) cs

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


{--------------------------------------------------------------------------
  system
--------------------------------------------------------------------------}

runSystemRaw :: String -> IO ()
runSystemRaw command
  = do -- putStrLn ("system: " ++ command)
       exitCode <- system command
       case exitCode of
         ExitFailure i -> raiseIO ("command failed:\n " ++ command )
         ExitSuccess   -> return ()

runSystem :: String -> IO ()
runSystem command
  = do -- putStrLn ("system: " ++ command)
       exitCode <- runSystemEx command
       case exitCode of
         ExitFailure i -> raiseIO ("command failed:\n " ++ command )
         ExitSuccess   -> return ()

runSystemEx command
  = system (FilePath.normalise command)

-- | Compare two file modification times (uses 0 for non-existing files)
fileTimeCompare :: FilePath -> FilePath -> IO Ordering
fileTimeCompare fname1 fname2
  = do time1 <- getFileTime fname1 
       time2 <- getFileTime fname2
       return (compare time1 time2)


maxFileTime :: FileTime -> FileTime -> FileTime
maxFileTime t1 t2
  = if (t1 >= t2) then t1 else t2

maxFileTimes :: [FileTime] -> FileTime
maxFileTimes times
  = foldr maxFileTime fileTime0 times

copyTextFile :: FilePath -> FilePath -> IO ()
copyTextFile src dest
  = if (src == dest)
     then return ()
     else catchIO (do createDirectoryIfMissing True (dirname dest)
                      copyFile src dest) 
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyTextFileWith :: FilePath -> FilePath -> (String -> String) -> IO ()
copyTextFileWith src dest transform
  = if (src == dest)
     then return ()
     else catchIO (do createDirectoryIfMissing True (dirname dest)
                      content <- readFile src
                      writeFile dest (transform content)) 
            (error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryFile :: FilePath -> FilePath -> IO ()
copyBinaryFile src dest
  = if (src == dest) 
     then return ()
     else catchIO (B.copyBinaryFile src dest) (\_ -> error ("could not copy file " ++ show src ++ " to " ++ show dest))

copyBinaryIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyBinaryIfNewer always srcName outName       
  = do ord <- if always then return GT else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyBinaryFile srcName outName
        else do -- putStrLn $ "no copy for: " ++ srcName ++ " to " ++ outName
                return ()

copyTextIfNewer :: Bool -> FilePath -> FilePath -> IO ()
copyTextIfNewer always srcName outName       
  = do ord <- if always then return GT else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFile srcName outName
        else do return ()

copyTextIfNewerWith :: Bool -> FilePath -> FilePath -> (String -> String) -> IO ()
copyTextIfNewerWith always srcName outName transform       
  = do ord <- if always then return GT else fileTimeCompare srcName outName
       if (ord == GT)
        then do copyTextFileWith srcName outName transform
        else do return ()


getInstallDir :: IO FilePath
getInstallDir
  = do p <- getProgramPath
       let d  = dirname p
           ds = splitDirectories d
           result = case reverse ds of
                      ("bin":es)   -> joinPaths (reverse es)
                      (_:"out":es) -> joinPaths (reverse es)
                      _            -> d
       -- trace ("install-dir: " ++ result ++ ": " ++ show ds) $
       return result               

       

getProgramPath :: IO FilePath
getProgramPath
  = do p <- C.getProgramPath  -- works on windows
       if (not (null p)) 
        then return p
        else do name <- getProgName
                if (null name)
                 then return "main"
                 else if (any isPathSep name)
                  then return name
                  else do paths <- getEnvPaths "PATH"
                          mbp   <- searchPaths paths [] name  -- search along the PATH
                          case mbp of
                            Just fname -> return fname
                            Nothing    -> return name


commonPathPrefix :: FilePath -> FilePath -> FilePath
commonPathPrefix s1 s2
  = joinPaths $ map fst $ takeWhile (\(c,d) -> c == d) $ zip (splitDirectories s1) (splitDirectories s2)


-- | Is a path absolute?
isAbsolute :: FilePath -> Bool
isAbsolute fpath
  = case fpath of
      (_:':':c:_) -> isPathSep c
      ('/':_)     -> True
      _           -> False

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

---------------------------------------------------------------
-- file searching
----------------------------------------------------------------

searchPaths :: [FilePath] -> [String] -> String -> IO (Maybe (FilePath))
searchPaths path exts name
  = fmap (fmap (\(root,name) -> joinPath root name)) (searchPathsEx path exts name)

searchPathsEx :: [FilePath] -> [String] -> String -> IO (Maybe (FilePath,FilePath))
searchPathsEx path exts name
  = search (concatMap (\dir -> map (\n -> (dir,n)) nameext) ("":path))
  where
    search [] = return Nothing  -- notfound envname nameext path
    search ((dir,fname):xs) 
      = do{ let fullName = joinPath dir fname
          ; exist <- doesFileExist fullName
          ; if exist
             then return (Just (dir,fname))
             else search xs
          }

    nameext
      = (nname : map (nname++) exts) 

    nname 
      = joinPaths $ dropWhile (==".") $ splitDirectories name

      
getEnvPaths :: String -> IO [FilePath]
getEnvPaths name
  = do{ xs <- getEnvVar name
      ; return (undelimPaths xs)
      }
  `catchIO` \err -> return []

getEnvVar :: String -> IO String
getEnvVar name
  = do env <- getEnvironment
       case lookup (map toLower name) (map (\(k,v) -> (map toLower k,v)) env) of
         Just val -> return val
         Nothing  -> return ""
