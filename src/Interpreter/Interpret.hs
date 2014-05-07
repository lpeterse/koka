------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{- |
  Interactive command prompt.
-}

module Interpreter.Interpret
  ( interpret
  ) where

import System.Directory            ( getCurrentDirectory, setCurrentDirectory )
import System.Cmd                  ( system )
import System.Exit                 ( ExitCode(..) )
import Data.List                   ( isPrefixOf )
import Data.Char                   ( isSpace )
import Control.Monad
import Control.Monad.IO.Class      ( MonadIO, liftIO )

import Platform.ReadLine      ( ReadLineT, runReadLineT, readLine )
import Lib.PPrint
import Lib.Printer
import Common.Failure         ( catchIO )
import Common.ColorScheme
import Common.File            ( joinPath )
import Common.Name            ( unqualify, qualify, newName )
import Common.NamePrim        ( nameExpr, nameType, nameInteractive, nameInteractiveModule, nameSystemCore )
import Common.Range
import Common.Error

import Syntax.Syntax

import Syntax.Highlight       ( highlightPrint )
import Kind.Synonym           ( synonymsIsEmpty,synonymsDiff, ppSynonyms )
import Kind.Assumption        ( kgammaFind, kgammaIsEmpty, ppKGamma )
import Kind.Pretty            ( prettyKind )
import Type.Assumption        ( gammaIsEmpty, ppGamma, infoType, gammaFilter )

import Compiler.Options
import Compiler.Compile
import Interpreter.Command
import Interpreter.State      ( State(..), reset )
import Interpreter.Load       ( loadFilesErr )
import Interpreter.Message    ( message
                              , messageLn
                              , messageLnLn
                              , messageInfoLnLn
                              , messagePrettyLn
                              , messageError
                              , messageErrorMsgLn
                              , messageErrorMsgLnLn
                              , messagePrettyLnLn
                              , messageMarker
                              , messageRemark
                              , messageEvaluation
                              , messageInfoLn
                              , messageHeader
                              , messageSchemeEffect
                              , messageScheme
                              , prettyScheme
                              , prettyEnv
                              )
import Interpreter.Quote      ( messageQuote )
import Interpreter.Editor     ( runEditor )

io :: MonadIO m => IO a -> m a
io = liftIO

{---------------------------------------------------------------
  Main
---------------------------------------------------------------}

-- | Loads the requested modules and goes into an evaluation loop prompting
--   the user for input to be evaluated.
interpret :: ColorPrinter   -- ^ supplies 'IO' actions for (coloured) output to stdout
             -> Flags       -- ^ flags for example from the command line
             -> [FilePath]  -- ^ files to load initially
             -> IO ()       -- ^
interpret printer' flags0 files'
  = runReadLineT
      $ do io $ messageHeader st
           err <- io $ loadFilesErr
                         (terminal st)
                         st { flags = flags0 { showCore = False, showAsmCSharp = False } }
                         [ show nameSystemCore ]
            -- FIXME: What does this catch? Why not returning it?
            `catchIO` \msg -> do messageError st msg;
                                 return $ errorMsg $ ErrorGeneral rangeNull (text msg)
           case checkError err of
             Left msg    -> do io $ messageInfoLn st ("unable to load the " ++ show nameSystemCore ++ " module; standard functions are not available")
                               io $ messageEvaluation st
                               interpreterEx st { flags      = (flags st){ evaluate = False }
                                                , errorRange = Just (getRange msg) }
             Right (preludeSt,_warnings)
               -> if (null files')
                    then interpreterEx preludeSt{ lastLoad = [] }
                    else command preludeSt (Load files')
  where
    st = State
          { printer       = printer'
          , flags         = flags0
          , evalDisable   = False
          , loaded0       = initialLoaded
          , loaded        = initialLoaded
          , defines       = []
          , program       = (programNull nameInteractiveModule)
          , errorRange    = Nothing
          , lastLoad      = []
          , loadedPrelude = initialLoaded
          }

{---------------------------------------------------------------
  Interpreter loop
---------------------------------------------------------------}

-- | A thin wrapper around the 'readLine' call that specifies the prompt
--   and a fallback command (:quit) in case 'readLine' returns 'Nothing'.
getCommand :: State -> ReadLineT IO Command
getCommand st
  = parseCommand `fmap` maybe ":quit" id `fmap` readLine ansiPrompt
  where
    -- Since readLine requires a string instead of an IO action
    -- we need to dispatch on the terminal type and assemble a string
    -- containing an ansi escape sequence if this is an ansi terminal.
    -- TODO: find a nicer solution. This is a layering violation.
    ansiEscape :: String -> String
    ansiEscape  = if isAnsiPrinter (printer st)
                    then ansiWithColor $ colorInterpreter
                                       $ colorSchemeFromFlags
                                       $ flags st
                    else id
    ansiPrompt :: String
    ansiPrompt  = ansiEscape "> "

-- | Tail-recursively calls 'interpreterEx' and clears 'errorRange'
interpreter :: State -> ReadLineT IO ()
interpreter st
  = do interpreterEx st'
  where
    st' = st{ errorRange = Nothing }

-- | Fetches a command and tail-recursively calls 'command' for evaluation
interpreterEx :: State -> ReadLineT IO ()
interpreterEx st
  = do cmd <- getCommand st
       command st cmd

-- | Interpret a command and (if not quit) recurses to 'interpreter'
command ::  State -> Command -> ReadLineT IO ()
command st cmd
  = case cmd of
      Eval ln     -> do{ err <- io $ compileExpression (terminal st) (flags st) (loaded st) (Executable nameExpr ()) (program st) bigLine ln
                       ; checkInfer st True err $ \ld -> 
                         do if (not (evaluate (flags st))) 
                             then let tp = infoType $ gammaFind (qualify nameInteractive nameExpr) (loadedGamma ld)
                                  in io $ messageSchemeEffect st tp
                             else io $ messageLn st ""
                            interpreter st{ loaded = ld } -- (loaded st){ loadedModules  = loadedModules ld }}
                       }

      Define ln   -> do err <- io $ compileValueDef (terminal st) (flags st) (loaded st) (program st) (lineNo st) ln
                        checkInfer2 st True err $ \(defName',ld) ->
                           do{ let tp    = infoType $ gammaFind defName' (loadedGamma ld)
                                   tpdoc = prettyScheme st tp
                             ; io $ messagePrettyLnLn st (text (show (unqualify defName')) <+> text ":" <+> tpdoc)
                             ; interpreter st{ program  = maybe (program st) id (modProgram (loadedModule ld))
                                             , loaded   = ld
                                             , defines  = filter (\(name,_) -> defName' /= name) (defines st)
                                                          ++ [(defName',[dropLet ln,""])]
                                             }
                             }

      TypeOf ln   -> do err <- io $ compileExpression (terminal st) (flags st) (loaded st) Object (program st) bigLine ln
                        checkInfer st True err $ \ld ->
                           do{ let tp = infoType $ gammaFind (qualify nameInteractive nameExpr) (loadedGamma ld)
                             ; io $ messageSchemeEffect st tp
                             ; interpreter st{ loaded = ld } -- (loaded st){ loadedModules  = loadedModules ld }}
                             }

      KindOf ln   -> do err <- io $ compileType (terminal st) (flags st) (loaded st) (program st) bigLine ln 
                        checkInfer st True err $ \ld ->
                           do{ let kind = kgammaFind (getName (program st)) nameType (loadedKGamma ld)
                             ; io $ messagePrettyLnLn st (prettyKind (colorSchemeFromFlags (flags st)) (snd kind))
                             ; interpreter st{ loaded = ld }
                             }

      TypeDef ln  -> -- trace ("modules: " ++ show (map (show . modName . loadedModule) (loadedModules st))) $
                     do err <- io $ compileTypeDef (terminal st) (flags st) (loaded st) (program st) (lineNo st) ln
                        checkInfer2 st True err $ \(defName', ld) ->
                         do{ let (_qname,kind) = kgammaFind (getName (program st)) defName'(loadedKGamma ld)
                           ; io $ messagePrettyLnLn st (text (show defName') <+> text "::" <+> pretty kind)
                           ; interpreter st{ program  = maybe (program st) id $ modProgram (loadedModule ld)
                                           , loaded   = ld
                                           , defines  = filter (\(name,_) -> defName' /= name) (defines st)
                                                        ++ [(defName',[ln,""])] 
                                           }
                           }

      Load fnames -> do{ let st' = st{ lastLoad = fnames } 
                       ; loadFiles (terminal st) st' (reset st') fnames 
                       }

      Reload      -> do{ loadFiles (terminal st) st (reset st) (lastLoad st) {- (map (modPath . loadedModule) (tail (loadedModules st))) -} }

      
      Edit []     -> do{ let fpath = lastFilePath st
                       ; if (null fpath)
                          then do io $ messageRemark st "nothing to edit"
                                  interpreterEx st
                          else do io $ runEditor st fpath
                                  command st Reload 
                       }
      Edit fname  -> do{ mbpath <- io $ searchSource (flags st) "" (newName fname) -- searchPath (includePath (flags st)) sourceExtension fname
                       ; case mbpath of
                          Nothing    
                            -> do io $ messageErrorMsgLnLn st (errorFileNotFound (flags st) fname)
                                  interpreter st
                          Just (root,fname') 
                            -> do io $ runEditor st (joinPath root fname')
                                  command st Reload 
                       }
      
      Shell cmd'  -> do{ ec <- io $ system cmd'
                       ; case ec of
                          ExitSuccess   -> io $ messageLn st ""
                          ExitFailure i -> io $ messageInfoLn st $ show i
                       ; interpreterEx st
                       }
      
      ChangeDir d -> do{ if (null d)
                          then do{ fpath <- io $ getCurrentDirectory
                                 ; io $ messageInfoLnLn st fpath
                                 }
                          else io $ setCurrentDirectory d
                       ; interpreterEx st
                       }

      Options opts-> do{ (newFlags,mode) <- io $ processOptions (flags st) (words opts)
                       ; let setFlags files'
                              = do if (null files')
                                    then io $ messageLn st ""
                                    else io $messageError st "(ignoring file arguments)"
                                   interpreter (st{ flags = newFlags })
                       ; case mode of
                           ModeHelp     -> do doc <- io $ commandLineHelp (flags st)
                                              io $ messagePrettyLn st doc
                                              interpreterEx st
                           ModeVersion  -> do io $ showVersion (printer st)
                                              io $ messageLn st ""
                                              interpreter st
                           ModeCompiler files'     -> setFlags files'
                           ModeInteractive files'  -> setFlags files'
                           -- ModeDoc files          -> setFlags files
                       }

      Error err   -> do{ io $ messageInfoLn st err
                       ; io $ messageInfoLn st "invalid command."
                       ; io $ messageInfoLnLn st "(type \":?\" for help on commands)"
                       ; interpreterEx st
                       }

      Show showcmd-> do{ io $ showCommand st showcmd
                       ; interpreterEx st
                       }
      
      Quit        -> do{ io $ messageQuote st
                       }

      None        -> do{ interpreterEx st }
  where
    lineNo :: State -> Int
    lineNo st'
      = bigLine + (length (defines st') + 1)

    loadFiles :: Terminal -> State -> State -> [FilePath] -> ReadLineT IO ()
    loadFiles term originalSt startSt files'
      = do err <- io $ loadFilesErr term startSt files'
           case checkError err of
             Left msg -> interpreterEx originalSt{ errorRange = Just (getRange msg) }
             Right (st',_warnings) -> interpreterEx st'

    errorFileNotFound :: Flags -> FilePath -> ErrorMessage 
    errorFileNotFound flgs name
      = ErrorIO (docNotFound (colorSchemeFromFlags flgs) (includePath flgs) name)

    docNotFound :: ColorScheme -> [FilePath] -> String -> Doc
    docNotFound cscheme path name
      = text "could not find:" <+> ppPath name <$>
        if (null path)
         then text ("search path empty. (use the \"-i\" flag at command line?)")
         else text "search path   :" <+> align (cat (punctuate comma (map ppPath path))) 
      where
        ppPath p
          = color (colorSource cscheme) (text p)

    -- | Only needed by the (Define ln) branch
    dropLet :: String -> String
    dropLet s
      = if isPrefixOf "let" s 
         then dropEndWhile (\c -> isSpace c || c == '}') (dropWhile (\c -> isSpace c || c == '{') (drop 3 s))
         else s
      where
        dropEndWhile p 
          = reverse . dropWhile p . reverse

    -- | Only needed by the (Edit []) branch
    lastFilePath :: State -> FilePath
    lastFilePath st'
       = let source = lastSource st'
         in if (isSourceNull source)
            then ""
            else sourceName source

{--------------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------------}

checkInfer ::  State -> Bool -> Error Loaded -> (Loaded -> ReadLineT IO ()) -> ReadLineT IO ()
checkInfer st = checkInferWith st id

checkInfer2 :: State -> Bool -> Error (t, Loaded) -> ((t, Loaded) -> ReadLineT IO ()) -> ReadLineT IO ()
checkInfer2 st = checkInferWith st (\(_,c) -> c)

checkInferWith ::  State -> (a -> Loaded) -> Bool -> Error a -> (a -> ReadLineT IO ()) -> ReadLineT IO ()
checkInferWith st _getLoaded showMarker err f
  = case checkError err of
      Left msg  -> do io $ when showMarker (maybeMessageMarker st (getRange msg))
                      io $ messageErrorMsgLnLn st msg
                      interpreterEx st{ errorRange = Just (getRange msg) }
      Right (x,ws)
                -> do let warnings = ws -- modWarnings (loadedModule ld)
                      io $ when (not (null warnings))
                        (do let msg = ErrorWarning warnings ErrorZero
                            when showMarker (maybeMessageMarker st (getRange msg))
                            messageErrorMsgLnLn st msg)
                      f x

maybeMessageMarker ::  State -> Range -> IO ()
maybeMessageMarker st rng
  = if (lineNo == posLine (rangeStart rng) || posLine (rangeStart rng) == bigLine)
     then messageMarker st rng
     else return ()
  where
    lineNo :: Int
    lineNo
      = bigLine + (length (defines st) + 1)

lastSource :: State -> Source
lastSource st
  = -- trace ("lastSource: " ++ show (map modSourcePath (loadedModules (loaded0 st))) ++ "," ++ modSourcePath (loadedModule (loaded0 st)) ++ ", " ++ show (errorRange st)) $
    let fsource = Source (head $ filter (not . null) $ map modSourcePath $  [loadedModule (loaded0 st)] ++ reverse (loadedModules $ loaded0 st))
                         bstringEmpty
        -- fsource = Source (modSourcePath (last (loadedModules (loaded0 st)))) bstringEmpty
        source  = case errorRange st of
                    Just rng -> let src = rangeSource rng
                                in if isSourceNull src 
                                    then fsource
                                    else src
                    Nothing  -> fsource
    in source



lastSourceFull :: State -> IO Source
lastSourceFull st
  = let source = lastSource st
    in if (isSourceNull source || not (null (sourceText source)))
        then return source
        else do txt <- readInput (sourceName source)
                          `catchIO` (\msg -> do{ messageError st msg; return bstringEmpty })
                return (source{ sourceBString = txt })

isSourceNull :: Source -> Bool
isSourceNull source
  = (sourceName source == show nameInteractiveModule || null (sourceName source))

{---------------------------------------------------------------
  Interprete a show command
---------------------------------------------------------------}

showCommand ::  State -> ShowCommand -> IO ()
showCommand st cmd
  = case cmd of
      ShowHelp
        -> do messagePrettyLn st $ commandHelp $ colorSchemeFromFlags $ flags st
              showEnv (flags st) (printer st)

      ShowVersion
        -> do showVersion (printer st) 
              messageLn st ""

      ShowKindSigs
        -> let kgamma = loadedKGamma (loaded st)
           in  if kgammaIsEmpty kgamma
                 then messageRemark st "no kinds to show"
                 else messagePrettyLnLn
                        st
                        ( ppKGamma
                            colors
                            ( loadedName (loaded st) )
                            ( loadedImportMap (loaded st) )
                            kgamma
                        )

      ShowTypeSigs
        -> let gamma = gammaFilter (modName (loadedModule (loaded st)))
                       $ loadedGamma (loaded st)
           in  if gammaIsEmpty gamma
                 then messageRemark st "no types to show"
                 else messagePrettyLnLn st (ppGamma (prettyEnv st) gamma)

      ShowSynonyms
        -> let syns = loadedDiff synonymsDiff loadedSynonyms
           in  if synonymsIsEmpty syns
                 then messageRemark st "no synonyms to show"
                 else messagePrettyLnLn st (ppSynonyms (prettyEnv st) syns)

      ShowSource
        -> do source <- lastSourceFull st
              if isSourceNull source
                then messageRemark st "no source code to show"
                else do syntaxColor source
                        messageLnLn st ""
                     -- messageLnLn st (sourceText (programSource (program st)))

      ShowDefines
        -> if null (defines st)
             then messageRemark st "no definitions to show"
             else do syntaxColor $ interactiveSource
                                 $ stringToBString
                                 $ unlines
                                 $ concatMap snd
                                 $ defines st
                  -- messagePrettyLn st (color (colorSource colors)
                  --         (vcat (concatMap (map string . snd) (defines st))))

  where
    colors :: ColorScheme
    colors
      = colorSchemeFromFlags (flags st)

    syntaxColor :: Source -> IO ()
    syntaxColor source
      = highlightPrint colors (sourceName source) 1 (sourceBString source) (printer st)

    interactiveSource :: BString -> Source
    interactiveSource str
      = Source (show nameInteractiveModule) str

    loadedDiff :: t -> (Loaded -> t1) -> t1
    loadedDiff _diff get
      = get (loaded st)

{--------------------------------------------------------------------------
  Misc
--------------------------------------------------------------------------}

-- | A terminal is a collection of pretty printing 'IO' actions.
terminal :: State -> Terminal
terminal st
  = Terminal
      ( messageErrorMsgLn st )
      ( if verbose (flags st) > 0
          then (\s -> withColor (printer st) DarkGray (message st (s ++ "\n")))
          else (\_ -> return ()))
      ( messagePrettyLn st )
      ( messageScheme st )
      ( messagePrettyLn st )
