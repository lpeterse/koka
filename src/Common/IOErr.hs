-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{- |
  The IOErr monad.
-}

module Common.IOErr ( IOErr
                    , runIOErr
                    , liftError
                    , liftIO
                    , lift
                    ) where

import Common.Error

data IOErr a = IOErr (IO (Error a))

instance Functor IOErr where
  fmap f (IOErr ie) = IOErr (fmap (fmap f) ie)

instance Monad IOErr where
  return x          = IOErr (return (return x))
  (IOErr ie) >>= f  = IOErr (do err <- ie
                                case checkError err of
                                   Right (x,w) -> case f x of
                                                   IOErr ie' -> do err <- ie'
                                                                   return (addWarnings w err)
                                   Left msg  -> return (errorMsg msg  ))

runIOErr :: IOErr a -> IO (Error a)
runIOErr (IOErr ie)
  = ie

liftError :: Error a -> IOErr a
liftError err
  = IOErr $ return err

liftIO :: IO a -> IOErr a
liftIO io
  = IOErr $ do x <- io
               return (return x)

lift :: IO (Error a) -> IOErr a
lift ie
  = IOErr ie