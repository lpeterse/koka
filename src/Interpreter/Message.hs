------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Interpreter.Message
  where

import Lib.PPrint
import Lib.Printer
import Common.ColorScheme
import Common.Range
import Common.Error
import Compiler.Options
import Compiler.Module        ( loadedName, loadedImportMap )
import qualified Platform.Config as Config
import Type.Type              ( Scheme )
import Type.Pretty            ( ppScheme, ppSchemeEffect, Env(context,importsMap))

import Interpreter.State

messageEvaluation :: Printer p => State p -> IO ()
messageEvaluation st
  = messageInfoLnLn st "evaluation is disabled"

messageErrorMsgLn :: Printer p => State p -> ErrorMessage -> IO ()
messageErrorMsgLn st err
  = messagePrettyLn st (ppErrorMessage (showSpan (flags st)) (colorSchemeFromFlags (flags st)) err)

messageErrorMsgLnLn :: Printer p => State p -> ErrorMessage -> IO ()
messageErrorMsgLnLn st err
  = messagePrettyLnLn st (ppErrorMessage (showSpan (flags st)) (colorSchemeFromFlags (flags st)) err)

messageError ::  Printer p => State p -> String -> IO ()
messageError st msg
  = messageInfoLnLn st msg

messageInfoLnLn ::  Printer p => State p -> String -> IO ()
messageInfoLnLn st s
  = do messageInfoLn st s
       messageLn st ""

messageInfoLn ::  Printer p => State p -> String -> IO ()
messageInfoLn st s
  = do messageInfo st s
       messageLn st ""     
       
messageInfo ::  Printer p => State p -> String -> IO ()
messageInfo st s
  = withColor (printer st) (colorInterpreter colors) (message st s)
  where
    colors
      = colorSchemeFromFlags (flags st)

messagePrettyLnLn ::  Printer p => State p -> Doc -> IO ()
messagePrettyLnLn st d
  = do messagePrettyLn st d
       messageLn st ""

messagePrettyLn ::  Printer p => State p -> Doc -> IO ()
messagePrettyLn st d
  = do messagePretty st d
       messageLn st ""

messagePretty ::  Printer p => State p -> Doc -> IO ()
messagePretty st d
  = writePretty (printer st) d

messageLnLn ::  Printer p => State p -> String -> IO ()
messageLnLn st s
  = messageLn st (s ++ "\n")

messageLn ::  Printer p => State p -> String -> IO ()
messageLn st s
  = writeLn (printer st) s

message ::  Printer p => State p -> String -> IO ()
message st s
  = write (printer st) s

messageRemark ::  Printer p => State p -> String -> IO ()
messageRemark st s
  = messageInfoLnLn st ("<" ++ s ++ ">")

messageMarker ::  Printer p => State p -> Range -> IO ()
messageMarker st rng
  = messagePrettyLn st makeMarker
  where
    colors
      = colorSchemeFromFlags (flags st)

    makeMarker
      = let c1 = posColumn (rangeStart rng)
        in color (colorMarker colors) (text (replicate (c1 + 1) ' ' ++ replicate 1 '^'))

messageHeader :: Printer p => State p -> IO ()
messageHeader st
  = messagePrettyLnLn st header
  where
    colors = colorSchemeFromFlags (flags st)
    header = color(colorInterpreter colors) $ vcat [
        text " _          _           "
       ,text "| |        | |          "
       ,text "| | __ ___ | | __ __ _  "
       ,text $ "| |/ // _ \\| |/ // _` | welcome to the " ++ Config.programName ++ " interpreter"
       ,text "|   <| (_) |   <| (_| | " <> headerVersion  
       ,text "|_|\\_\\\\___/|_|\\_\\\\__,_| " <> color (colorSource colors) (text "type :? for help") 
       ]
    headerVersion = text $ "version " ++ Config.version ++ (if Config.buildVariant /= "release" then (" (" ++ Config.buildVariant ++ ")") else "") ++ ", " ++ Config.buildDate

messageScheme ::  Printer p => State p -> Scheme -> IO ()
messageScheme st tp
  = messagePrettyLnLn st (prettyScheme st tp)

prettyScheme :: Printer p => State p -> Scheme -> Doc
prettyScheme st tp
  = ppScheme (prettyEnv st) tp

messageSchemeEffect ::  Printer p => State p -> Scheme -> IO ()
messageSchemeEffect st tp
  = messagePrettyLnLn st (prettySchemeEffect st tp)

prettySchemeEffect :: Printer p => State p -> Scheme -> Doc
prettySchemeEffect st tp
  = ppSchemeEffect (prettyEnv st) tp

-- | See "Type.Pretty" for details.
prettyEnv :: Printer p => State p -> Env
prettyEnv st
  = ( prettyEnvFromFlags (flags st) )
      { context    = loadedName (loaded st)
      , importsMap = loadedImportMap (loaded st)
      }