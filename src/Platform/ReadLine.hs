{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports readline functionality
-}
-----------------------------------------------------------------------------
module Platform.ReadLine( ReadLineT, runReadLineT, readLine, readLineEx
                        ) where

#define CLI_HASKELINE 2

#if CLI == CLI_HASKELINE

import System.Console.Haskeline

type ReadLineT m a = InputT m a

runReadLineT :: ReadLineT IO a -> IO a
runReadLineT
  = runInputT defaultSettings

readLine     :: String -> ReadLineT IO (Maybe String)
readLine prompt
  = getInputLine prompt

readLineEx   :: String -> a -> ReadLineT IO (Maybe String)
readLineEx prompt putPrompt
  = getInputLine prompt

#else

import System.IO

type ReadLineT m a = m a

runReadLineT :: ReadLineT IO a -> IO a
runReadLineT io
  = io

readLine     :: String -> ReadLineT IO (Maybe String)
readLine prompt
  = readLineEx prompt (do{ putStr prompt; hFlush stdout})

readLineEx   :: String -> ReadLineT IO () -> ReadLineT IO (Maybe String)
readLineEx prompt putPrompt
  = do s <- readLines
       return (Just s)
  where
    readLines
      = do putPrompt
           line <- getLine
           case reverse line of
             []       -> readLines
             '\\' : t -> do line2 <- readLines
                            return (reverse t ++ "\n" ++ line2)
             _        -> return line

#endif
