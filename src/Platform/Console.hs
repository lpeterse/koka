{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- for ghc older than 7.0: {-# OPTIONS -fglasgow-exts -#include "cconsole.h" #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module for portable control of colors in a console.
    Warning: "Lib.Printer" depends strongly on implicit interface
    assumptions.
-}
-----------------------------------------------------------------------------
module Platform.Console( setColor, setBackColor, setReverse, setUnderline
                       , withConsole, bracketConsole
                       ) where

#if defined(__WIN32__) || defined(__MINGW32__) || defined(__CYGWIN__)

import Platform.Runtime( finally )
import System.IO       ( hFlush, stdout )
import Foreign.C.String( CString, peekCString )

setColor :: Enum c => c -> IO ()
setColor c
  = do hFlush stdout
       consoleSetColor (fromEnum c)

setBackColor :: Enum c => c -> IO ()
setBackColor c
  = do hFlush stdout
       consoleSetBackColor (fromEnum c)

setReverse :: Bool -> IO ()
setReverse r
  = do hFlush stdout
       consoleSetReverse (if r then 1 else 0)

setUnderline :: Bool -> IO ()
setUnderline u
  = do hFlush stdout
       consoleSetUnderline (if u then 1 else 0)

-- | Initialize the console module. Passes 'True' on success.
withConsole :: (Bool -> IO a) -> IO a
withConsole f
  = do success <- consoleInit
       if (success==0)
        then f False
        else finally (f True) (do hFlush stdout; consoleDone)

-- | Restore the console state after a computation
bracketConsole :: IO a -> IO a
bracketConsole io
  = do con <- consoleGetState
       finally io (do hFlush stdout
                      consoleSetState con)

foreign import ccall consoleInit      :: IO Int
foreign import ccall consoleDone      :: IO ()
foreign import ccall consoleGetState  :: IO Int
foreign import ccall consoleSetState  :: Int -> IO ()
foreign import ccall consoleSetColor      :: Int -> IO ()
foreign import ccall consoleSetBackColor  :: Int -> IO ()
foreign import ccall consoleSetReverse    :: Int -> IO ()
foreign import ccall consoleSetUnderline  :: Int -> IO ()

#else

setColor         :: Enum c => c -> IO ()
setColor c        = return ()

setBackColor     :: Enum c => c -> IO ()
setBackColor c    = return ()

setReverse       :: Bool -> IO ()
setReverse b      = return ()

setUnderline     :: Bool -> IO ()
setUnderline b    = return ()

withConsole      :: (Bool -> IO a) -> IO a
withConsole f     = f False

bracketConsole   :: IO a -> IO a
bracketConsole io = io

#endif
