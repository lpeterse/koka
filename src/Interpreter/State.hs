------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Interpreter.State
  ( State(..)
  , reset
  ) where

import Lib.Printer            ( ColorPrinter )
import Common.Name            ( Name )
import Common.NamePrim        ( nameInteractiveModule )
import Common.Range           ( Range )

import Syntax.Syntax          ( UserProgram, programNull )

import Compiler.Module        ( Loaded, initialLoaded )
import Compiler.Options       ( Flags(..) )

data State = State{  printer       :: ColorPrinter
                   -- system variables
                   , flags         :: Flags
                   , evalDisable   :: Bool
                   -- program state
                   , loaded0       :: Loaded            -- ^ load state just after :l command
                   , loaded        :: Loaded            -- ^ load state with interactive defs
                   , defines       :: [(Name,[String])] -- ^ interactive definitions
                   , program       :: UserProgram       -- ^ interactive definitions as a program
                   , errorRange    :: Maybe Range       -- ^ last error location
                   , lastLoad      :: [FilePath]        -- ^ last load command
                   , loadedPrelude :: Loaded            -- ^ load state after loading the prelude
                   }

reset :: State -> State
reset st
  =  st{ program       = programNull nameInteractiveModule
       , defines       = [] 
       , loaded        = loadedPrelude st -- initialLoaded 
       , loaded0       = if rebuild (flags st)
                           then initialLoaded
                           else loaded0 st
       , errorRange    = Nothing
       }
