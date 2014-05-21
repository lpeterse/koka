------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Interpreter.Quote
  ( messageQuote )
  where

import System.Random

import Lib.Printer

import Interpreter.State
import Interpreter.Message ( messageInfoLnLn )

messageQuote :: Printer p => State p -> IO ()
messageQuote st
  = do idx <- randomRIO (0,length quotes-1)
       let (quote,author) = quotes!!idx
       messageInfoLnLn st ("\n" ++ quote ++ "\n -- " ++ author)
  where
    quotes :: [(String,String)]
    quotes
      = [("The cause is hidden. The effect is visible to all.","Ovid (43 BC - 17 AD)") 
        ,("I think of my body as a side effect of my mind.", "Carrie Fisher (1956)")
        ,("Laughter is a tranquilizer with no side effects.", "Arnold H. Glasgow")
        ,("To the extent this helps us in any sort of competition, that's great,\nbut that's actually a side effect. It's a happy side effect, nonetheless.", "Gary Flake")
        ,("There are no side effects -- only effects.\nThose we thought of in advance, the ones we like, we call\nthe main, or intended, effects, and take credit for them.\nThe ones we didn't anticipate,the ones that came around\nand bit us in the rear -- those are the `side effects'", "John D. Sterman (2002)")
        ,("Many people recognize that technology often comes\nwith unintended and undesirable side effects.","Leon Kass")
        ,("Before the effect one believes in different causes than one does after the effect.","Friedrich Nietzsche")
        ,("The cause ceasing, the effect ceases also.","Edward Coke")
        ,("Logic can often be reversed, but the effect does not precede the cause.","Gregory Bateson")
        -- ,("Most exciting ideas are not important.\nMost important ideas are not exciting.\nNot every problem has a solution.\nEvery solution has side effects.", "Daniel E. Geer Jr.")
        -- ,("Every cause produces more than one effect.","Herbert Spencer")
        -- ,("No action is without its side effects.","Barry Commoner")
        ]
