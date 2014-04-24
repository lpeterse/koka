{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Configuration data
-}
-----------------------------------------------------------------------------
module Platform.Config where

#if defined(__CABAL__)
import Data.Version
import qualified Paths_koka as P
#endif

-- by not inlining these we avoid rebuilding too many source files (since the .hi stays unchanged)
{-# NOINLINE version #-}
{-# NOINLINE buildDate #-}
{-# NOINLINE buildTime #-}

version :: String
#if   defined(__CABAL__)
version = showVersion P.version
#elif defined(VERSION)
version = VERSION
#else
version = "?"
#endif

programName :: String
#ifdef MAIN  
programName = MAIN
#else
programName = "koka"
#endif

buildVariant :: String
#ifdef VARIANT
buildVariant = VARIANT
#elif __CABAL__
buildVariant = "cabalized"
#else
buildVariant = "unknown"
#endif

compiler :: String
#ifdef COMPILER
compiler = COMPILER
#elif __GHCI__
compiler = "ghci"
#elif __GLASGOW_HASKELL__
compiler = "ghc"
#else
compiler = "unknown"
#endif

exeExtension   :: String
pathSep,pathDelimiter :: Char
#if defined(__WIN32__) || defined(__MINGW32__) || defined(__CYGWIN__)
exeExtension  = ".exe"
libExtension  = ".dll"
pathSep       = '\\'
pathDelimiter = ';'
#else
exeExtension  = ""
libExtension  = ".o"
pathSep       = '/'
pathDelimiter = ':'
#endif

sourceExtension :: String
sourceExtension = ".kk"

ifaceExtension  :: String
ifaceExtension  = ".kki"

buildDate :: String
buildDate  = __DATE__

buildTime :: String
buildTime  = __TIME__ ++ " " ++ __DATE__

getDataPath :: IO String
#if defined(__CABAL__)
getDataPath = P.getDataFileName ""
#else
getDataPath = return "."
#endif