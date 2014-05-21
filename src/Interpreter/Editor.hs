------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Interpreter.Editor
  ( runEditor
  , runEditorAt
  ) where

import System.Cmd                  ( system )

import Lib.Printer

import Common.Range

import Compiler.Options            ( editor )

import Interpreter.State
import Interpreter.Message

runEditor ::  Printer p => State p -> FilePath -> IO ()
runEditor st fpath
  = let (row,col) = case errorRange st of
                      Just rng  | sourceName (rangeSource rng) == fpath
                        -> let pos = rangeStart rng in (posLine pos, posColumn pos)
                      _ -> (1,1)
    in runEditorAt st fpath row col

runEditorAt ::  Printer p => State p -> FilePath -> Int -> Int -> IO ()
runEditorAt st fpath row col
  = let cmd  = replace $ editor (flags st)
    in if null (editor (flags st))
        then messageError st ("no editor specified. (use the \"koka-editor\" environment variable?)")
        else do _ <- system cmd
                return ()
  where
    replace :: String -> String
    replace s
      = walk True s
    qfpath
      = fpath
    walk add xs
      = let (pre,post) = span (/='%') xs
        in case post of
             ('%':'c':cs) -> pre ++ show col ++ walk add cs
             ('%':'l':cs) -> pre ++ show row ++ walk add cs
             ('%':'f':cs) -> pre ++ qfpath ++ walk False cs
             (c:cs)       -> pre ++ [c] ++ walk add cs
             []           -> pre ++ (if add then " " ++ qfpath else "")
