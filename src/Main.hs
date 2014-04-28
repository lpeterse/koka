------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{----------------------------------------------------------------------------
    Main module
----------------------------------------------------------------------------}

module Main where

import System.Exit            ( exitFailure )
import Control.Monad          ( when )

import Lib.PPrint             ( Pretty(pretty), writePrettyLn )
import Lib.Printer            ( writeLn, ColorPrinter, withColorPrinter, withFileNoColorPrinter, withNoColorPrinter, withHtmlColorPrinter )
import Common.Error           ( ErrorMessage(..), ppErrorMessage, checkError )
import Common.Name            ( newName, nameNil )
import qualified Compiler.Options as Options
import Compiler.Compile       ( compileFile, CompileTarget(..), Module(..), Loaded(..), Terminal(..) )
import Interpreter.Interpret  ( interpret  )
import Kind.ImportMap         ( importsEmpty )
import Kind.Synonym           ( synonymsIsEmpty, ppSynonyms, synonymsFilter )
import Kind.Assumption        ( kgammaFilter )
import Type.Assumption        ( ppGamma, gammaFilter )
import Type.Pretty            ( ppScheme, Env(context,importsMap) )

-- | Entrypoint when running as binary.
main     :: IO ()
main      = mainArgs ""

-- | Alternative (interactive) entry points
maing    :: IO ()
maing     = mainArgs "-ilib -itest --verbose"
maindoc  :: IO ()
maindoc   = mainArgs "-ilib -itest --verbose --html"
mainjs   :: IO ()
mainjs    = mainArgs "-ilib -itest --verbose --target=js"
maincs   :: IO ()
maincs    = mainArgs "-ilib -itest --verbose --target=cs"
mainh    :: IO ()
mainh     = mainArgs "-ilib -itest --console=raw"

-- | Parse options, dispatch terminal type, call compiler/interpreter.
mainArgs :: String -> IO ()
mainArgs args
  = do  (flags, mode) <- Options.getOptions args
        let withPrinter | not $ null $ Options.redirectOutput flags
                        = withFileNoColorPrinter (Options.redirectOutput flags)
                        | Options.console flags == "html"
                        = withHtmlColorPrinter
                        | Options.console flags == "ansi"
                        = withColorPrinter
                        | otherwise
                        = withNoColorPrinter
        withPrinter (mainMode flags mode)

-- | Decides based on 'Options.Mode' whether to run as compiler/interpreter/etc.
mainMode :: Options.Flags -> Options.Mode -> ColorPrinter -> IO ()
mainMode flags mode p
  = case mode of
      Options.ModeHelp
       -- just show the the help, then quit
       -> Options.showHelp flags p
      Options.ModeVersion
       -- just show the version, then quit
       -> Options.showVersion p
      Options.ModeCompiler files
       -- independently invoke the compiler once per input file
       -> mapM_ (compileAndPrintErrors p flags) files
      Options.ModeInteractive files
       -- interactive mode gets the user an interactive shell
       -- this call returns if the user explicitly quits the session
       -> interpret p flags files

-- | Compile a single file designated by filename and print errors
compileAndPrintErrors :: ColorPrinter -> Options.Flags -> FilePath -> IO ()
compileAndPrintErrors p flags fname
  = do let exec = Executable (newName "main") ()
       err <- compileFile
                term
                flags
                []
                ( if not (Options.evaluate flags)
                    then ( if Options.library flags
                             then Library
                             else exec
                         )
                    else exec
                )
                fname
       case checkError err of
         Left msg
           -> do putPrettyLn $ ppErrorMessage (Options.showSpan flags) cscheme msg
                 exitFailure
         Right ( Loaded
                  gamma
                  kgamma
                  synonyms
                  _newtypes
                  _constructors
                  _
                  imports
                  _
                  ( Module modName' _ _ _ _ _ _rawProgram _core _ _modTime )
                  _
               , warnings )
           -> do when (not $ null warnings)
                   $ putPrettyLn
                   $ ppErrorMessage (Options.showSpan flags) cscheme
                   $ ErrorWarning warnings ErrorZero

                 when (Options.showKindSigs flags)
                   $ do putPrettyLn $ pretty $ kgammaFilter modName' kgamma
                        let localSyns = synonymsFilter modName' synonyms
                        when (not $ synonymsIsEmpty localSyns)
                          $ putPrettyLn
                          $ ppSynonyms (prettyEnv modName' imports) localSyns

                 when (Options.showTypeSigs flags)
                   $ putPrettyLn
                   $ ppGamma (prettyEnv modName' imports)
                   $ gammaFilter modName' gamma
  where
    term
      = Terminal (putErrorMessage (Options.showSpan flags)) 
                 (if (Options.verbose flags > 1) then putStrLn else (\_ -> return ()))  
                 (if (Options.verbose flags > 0) then writePrettyLn p else (\_ -> return ()))
                 (putScheme $ prettyEnv nameNil importsEmpty) 
                 (writePrettyLn p)
    cscheme
      = Options.colorSchemeFromFlags flags
    prettyEnv ctx imports
      = (Options.prettyEnvFromFlags flags){ context = ctx, importsMap = imports }
    putScheme env tp
      = putPrettyLn (ppScheme env tp)
    putErrorMessage endToo err
      = putPrettyLn (ppErrorMessage endToo cscheme err)
    putPrettyLn doc
      = do writePrettyLn p doc
           writeLn p ""
