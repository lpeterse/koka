-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{- |
  Loads module files and inserts them into the interpreter state.
-}

module Interpreter.Load
  ( loadFilesErr
  ) where

import Lib.Printer
import Common.Range
import Common.Error
import Common.NamePrim
import Compiler.Compile
import Common.Syntax
import Syntax.Syntax

import Interpreter.State
import Interpreter.Message

loadFilesErr :: Printer p => Terminal -> State p -> [FilePath] -> IO (Error (State p))
loadFilesErr term startSt fileNames
  = do walk [] startSt fileNames
  where
    walk :: Printer p => [Module] -> State p -> [FilePath] -> IO (Error (State p))
    walk imports st files'
      = case files' of
          []  -> do if (not (null imports))
                      then do messageInfoLn st "modules:"
                              sequence_ [messageLn st ("  " ++ show (modName m) ) | m <- imports ]

                      else return () -- messageRemark st "nothing to load"
                    messageLn st ""
                    let st' = st{ program = programAddImports (program st) (map toImport imports) }
                        toImport mod'
                            = Import (modName mod') (modName mod') rangeNull Private
                    return (return st')
          (fname:fnames)
              -> do{ err <- {- if (False) -- any isPathSep fname) 
                             then compileFile term (flags st) (loadedModules (loaded0 st)) Object fname
                             else compileModule term (flags st) (loadedModules (loaded0 st)) (newName fname)
                             -}
                             compileModuleOrFile term (flags st) (loadedModules (loaded0 st)) fname
                   ; case checkError err of 
                       Left msg 
                          -> do messageErrorMsgLnLn st msg
                                return (errorMsg msg) 
                       Right (ld,warnings)
                          -> do{ -- let warnings = modWarnings (loadedModule ld)
                               ; err' <- if not (null warnings)
                                         then do let msg = ErrorWarning warnings ErrorZero
                                                 messageErrorMsgLn st msg
                                                 return (Just (getRange msg))
                                         else do return (errorRange st)
                               ; let newst = st{ loaded        = ld
                                               , loaded0       = ld
                                               -- , modules       = modules st ++ [(fname,loadedModule ld)]
                                               , errorRange    = err'
                                               , loadedPrelude = if (modName (loadedModule ld) == nameSystemCore ) 
                                                                  then ld else loadedPrelude st
                                               }
                               ; let modl = loadedModule ld
                               ; walk (if (modName modl == nameSystemCore) then imports else (modl : imports)) newst fnames
                               }
                   }