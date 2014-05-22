{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
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
module Platform.ReadLine( InputT, MonadException, InputM(..), runInput
                        ) where

#define CLI_HASKELINE 2

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class ()

#if CLI == CLI_HASKELINE

import System.Console.Haskeline

runInput :: MonadException m => InputT m a -> m a
runInput
  = runInputT defaultSettings


instance (MonadIO m, MonadException m) => InputM (InputT m) where
  readLine = getInputLine

#else

import System.IO

newtype InputT m a
      = InputT (m a)
      deriving (Functor, Applicative, Monad, MonadIO, MonadException)

runInput :: InputT m a -> m a
runInput (InputT m)
  = m

class (MonadIO m) => MonadException m
instance MonadException IO
instance (MonadIO m) => MonadException (ReaderT p m)

instance (MonadIO m, MonadException m) => InputM (InputT m) where
  readLine prompt
    = liftIO $ do s <- readLines
                  return (Just s)
    where
      putPrompt
        = do putStr prompt
             hFlush stdout
      readLines
        = do putPrompt
             line <- getLine
             case reverse line of
               []       -> readLines
               '\\' : t -> do line2 <- readLines
                              return (reverse t ++ "\n" ++ line2)
               _        -> return line

#endif

class (MonadIO m, MonadException m) => InputM m where
  readLine :: String -> m (Maybe String)
