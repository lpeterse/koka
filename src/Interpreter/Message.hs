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
import qualified Platform.Config as Config

import Interpreter.State

messageEvaluation :: State -> IO ()
messageEvaluation st
  = messageInfoLnLn st "evaluation is disabled"

messageErrorMsgLn :: State -> ErrorMessage -> IO ()
messageErrorMsgLn st err
  = messagePrettyLn st (ppErrorMessage (showSpan (flags st)) (colorSchemeFromFlags (flags st)) err)

messageErrorMsgLnLn :: State -> ErrorMessage -> IO ()
messageErrorMsgLnLn st err
  = messagePrettyLnLn st (ppErrorMessage (showSpan (flags st)) (colorSchemeFromFlags (flags st)) err)

messageError ::  State -> String -> IO ()
messageError st msg
  = messageInfoLnLn st msg

messageInfoLnLn ::  State -> String -> IO ()
messageInfoLnLn st s
  = do messageInfoLn st s
       messageLn st ""

messageInfoLn ::  State -> String -> IO ()
messageInfoLn st s
  = do messageInfo st s
       messageLn st ""     
       
messageInfo ::  State -> String -> IO ()
messageInfo st s
  = withColor (printer st) (colorInterpreter colors) (message st s)
  where
    colors
      = colorSchemeFromFlags (flags st)

messagePrettyLnLn ::  State -> Doc -> IO ()
messagePrettyLnLn st d
  = do messagePrettyLn st d
       messageLn st ""

messagePrettyLn ::  State -> Doc -> IO ()
messagePrettyLn st d
  = do messagePretty st d
       messageLn st ""

messagePretty ::  State -> Doc -> IO ()
messagePretty st d
  = writePretty (printer st) d

messageLnLn ::  State -> String -> IO ()
messageLnLn st s
  = messageLn st (s ++ "\n")

messageLn ::  State -> String -> IO ()
messageLn st s
  = writeLn (printer st) s

message ::  State -> String -> IO ()
message st s
  = write (printer st) s

messageRemark ::  State -> String -> IO ()
messageRemark st s
  = messageInfoLnLn st ("<" ++ s ++ ">")

messageMarker ::  State -> Range -> IO ()
messageMarker st rng
  = messagePrettyLn st makeMarker
  where
    colors
      = colorSchemeFromFlags (flags st)

    makeMarker
      = let c1 = posColumn (rangeStart rng)
        in color (colorMarker colors) (text (replicate (c1 + 1) ' ' ++ replicate 1 '^'))

messageHeader :: State -> IO ()
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
