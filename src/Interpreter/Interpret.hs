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

module Interpreter.Interpret( interpret ) where

import System.Random
import System.Directory            ( getCurrentDirectory, setCurrentDirectory )
import System.Cmd                  ( system )
import System.Exit                 ( ExitCode(..) )
import Data.List                   ( isPrefixOf )
import Data.Char                   ( isSpace )
import Control.Monad
import Control.Monad.IO.Class      ( MonadIO, liftIO )

import Platform.Config hiding ( programName )
import qualified Platform.Config as Config
import Platform.ReadLine      ( ReadLineT, runReadLineT, readLineEx )
import Lib.PPrint
import Lib.Printer
import Common.Failure         ( raiseIO, catchIO )
import Common.ColorScheme
import Common.File            ( joinPath )
import Common.Name            ( Name, unqualify, qualify, newName )
import Common.NamePrim        ( nameExpr, nameType, nameInteractive, nameInteractiveModule, nameSystemCore )
import Common.Range
import Common.Error

import Common.Syntax
import Syntax.Syntax

import Syntax.Highlight       ( highlightPrint )
import Kind.Synonym           ( synonymsIsEmpty,synonymsDiff, ppSynonyms )
import Kind.Assumption        ( kgammaFind, kgammaIsEmpty, ppKGamma )
import Kind.Pretty            ( prettyKind )
import Type.Type              ( Scheme )
import Type.Pretty            ( ppScheme, ppSchemeEffect, Env(context,importsMap))
import Type.Assumption        ( gammaIsEmpty, ppGamma, infoType, gammaFilter )

import Compiler.Options
import Compiler.Compile
import Interpreter.Command

{---------------------------------------------------------------
  interpreter state
---------------------------------------------------------------}

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
      Eval ln     -> do{ err <- io $ compileExpression term (flags st) (loaded st) (Executable nameExpr ()) (program st) bigLine ln
                       ; checkInfer st True err $ \ld -> 
                         do if (not (evaluate (flags st))) 
                             then let tp = infoType $ gammaFind (qualify nameInteractive nameExpr) (loadedGamma ld)
                                  in io $ messageSchemeEffect st tp
                             else io $ messageLn st ""
                            interpreter st{ loaded = ld } -- (loaded st){ loadedModules  = loadedModules ld }}
                       }

      Define ln   -> do err <- io $ compileValueDef term (flags st) (loaded st) (program st) (lineNo st) ln
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

      TypeOf ln   -> do err <- io $ compileExpression term (flags st) (loaded st) Object (program st) bigLine ln
                        checkInfer st True err $ \ld ->
                           do{ let tp = infoType $ gammaFind (qualify nameInteractive nameExpr) (loadedGamma ld)
                             ; io $ messageSchemeEffect st tp
                             ; interpreter st{ loaded = ld } -- (loaded st){ loadedModules  = loadedModules ld }}
                             }

      KindOf ln   -> do err <- io $ compileType term (flags st) (loaded st) (program st) bigLine ln 
                        checkInfer st True err $ \ld ->
                           do{ let kind = kgammaFind (getName (program st)) nameType (loadedKGamma ld)
                             ; io $ messagePrettyLnLn st (prettyKind (colorSchemeFromFlags (flags st)) (snd kind))
                             ; interpreter st{ loaded = ld }
                             }

      TypeDef ln  -> -- trace ("modules: " ++ show (map (show . modName . loadedModule) (loadedModules st))) $
                     do err <- io $ compileTypeDef term (flags st) (loaded st) (program st) (lineNo st) ln
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
                       ; loadFiles term st' (reset st') fnames 
                       }

      Reload      -> do{ loadFiles term st (reset st) (lastLoad st) {- (map (modPath . loadedModule) (tail (loadedModules st))) -} }

      
      Edit []     -> do{ let fpath = lastFilePath st
                       ; if (null fpath)
                          then do io $ remark st "nothing to edit"
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
      
      Quit        -> do{ io $ putQuote st
                       }

      None        -> do{ interpreterEx st }
  where
    term = terminal st

{--------------------------------------------------------------------------
  File loading
--------------------------------------------------------------------------}

loadFiles :: Terminal -> State -> State -> [FilePath] -> ReadLineT IO ()
loadFiles term originalSt startSt files'
  = do err <- io $ loadFilesErr term startSt files'
       case checkError err of
         Left msg -> interpreterEx originalSt{ errorRange = Just (getRange msg) }
         Right (st,_warnings) -> interpreterEx st 

loadFilesErr :: Terminal -> State -> [FilePath] -> IO (Error State)
loadFilesErr term startSt fileNames
  = do walk [] startSt fileNames
  where
    walk :: [Module] -> State -> [FilePath] -> IO (Error State)
    walk imports st files'
      = case files' of
          []  -> do if (not (null imports))
                      then do messageInfoLn st "modules:"
                              sequence_ [messageLn st ("  " ++ show (modName m) ) | m <- imports ]

                      else return () -- remark st "nothing to load"                      
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
  = if (lineNo st == posLine (rangeStart rng) || posLine (rangeStart rng) == bigLine)
     then messageMarker st rng
     else return ()

lineNo :: State -> Int
lineNo st
  = bigLine + (length (defines st) + 1)

dropLet :: [Char] -> [Char]
dropLet s
  = if isPrefixOf "let" s 
     then dropEndWhile (\c -> isSpace c || c == '}') (dropWhile (\c -> isSpace c || c == '{') (drop 3 s))
     else s
  where
    dropEndWhile p 
      = reverse . dropWhile p . reverse

messageScheme ::  State -> Scheme -> IO ()
messageScheme st tp
  = do messagePrettyLnLn st (prettyScheme st tp)

prettyScheme :: State -> Scheme -> Doc
prettyScheme st tp
  = ppScheme (prettyEnv st) tp

messageSchemeEffect ::  State -> Scheme -> IO ()
messageSchemeEffect st tp
  = do messagePrettyLnLn st (prettySchemeEffect st tp)

prettySchemeEffect :: State -> Scheme -> Doc
prettySchemeEffect st tp
  = ppSchemeEffect (prettyEnv st) tp

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

lastFilePath :: State -> [Char]
lastFilePath st
   = let source = lastSource st
     in if (isSourceNull source)
        then ""
        else sourceName source

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

loadedDiff :: t -> (Loaded -> t1) -> State -> t1
loadedDiff _diff get st
  = get (loaded st)

prettyEnv :: State -> Env
prettyEnv st
  = (prettyEnvFromFlags (flags st)){ context = loadedName (loaded st), importsMap = loadedImportMap (loaded st) } 

showCommand ::  State -> ShowCommand -> IO ()
showCommand st cmd
  = case cmd of
      ShowHelp         -> do messagePrettyLn st (commandHelp (colorSchemeFromFlags (flags st)))
                             showEnv (flags st) (printer st)

      ShowVersion      -> do showVersion (printer st) 
                             messageLn st ""

      ShowKindSigs     -> let kgamma = {- loadedDiff kgammaDiff -} loadedKGamma (loaded st)
                          in if (kgammaIsEmpty kgamma)
                           then remark st "no kinds to show"
                           else messagePrettyLnLn st (ppKGamma colors (loadedName (loaded st)) (loadedImportMap (loaded st)) kgamma)

      ShowTypeSigs     -> let gamma = gammaFilter (modName (loadedModule (loaded st))) $ loadedGamma (loaded st)
                          in if (gammaIsEmpty gamma) 
                           then remark st "no types to show"
                           else messagePrettyLnLn st (ppGamma (prettyEnv st) gamma)

      ShowSynonyms     -> let syns = loadedDiff synonymsDiff loadedSynonyms st
                          in if (synonymsIsEmpty syns)
                           then remark st "no synonyms to show"
                           else messagePrettyLnLn st 
                                  (ppSynonyms (prettyEnv st) syns)

      ShowSource       -> do source <- lastSourceFull st
                             if (isSourceNull source)
                              then remark st "no source code to show"
                              else do syntaxColor source
                                      messageLnLn st ""
                                      -- messageLnLn st (sourceText (programSource (program st)))

      ShowDefines      -> if (null (defines st))
                           then remark st "no definitions to show"
                           else syntaxColor (interactiveSource (stringToBString (unlines (concatMap snd (defines st)))))
                                -- messagePrettyLn st (color (colorSource colors)
                                --                     (vcat (concatMap (map string . snd) (defines st))))


  where
    colors
      = colorSchemeFromFlags (flags st)

    syntaxColor source
      = do highlightPrint colors (sourceName source) 1 (sourceBString source) (printer st)

    interactiveSource :: BString -> Source
    interactiveSource str
      = Source (show (nameInteractiveModule)) str

{--------------------------------------------------------------------------
  Run an editor
--------------------------------------------------------------------------}
runEditor ::  State -> FilePath -> IO ()
runEditor st fpath
  = let (row,col) = case errorRange st of
                      Just rng  | sourceName (rangeSource rng) == fpath
                        -> let pos = rangeStart rng in (posLine pos, posColumn pos)
                      _ -> (1,1)
    in runEditorAt st fpath row col

runEditorAt ::  State -> FilePath -> Int -> Int -> IO ()
runEditorAt st fpath row col
  = let cmd  = replace row col (editor (flags st)) fpath 
    in if null (editor (flags st))
        then raiseIO ("no editor specified. (use the \"koka-editor\" environment variable?)")
        else do _ <- system cmd
                return ()
        
replace :: Int -> Int -> FilePath -> String -> String
replace row col s fpath
  = walk True s
  where
    qfpath  = fpath 
    walk add xs
      = let (pre,post) = span (/='%') xs
        in case post of
             ('%':'c':cs) -> pre ++ show col ++ walk add cs
             ('%':'l':cs) -> pre ++ show row ++ walk add cs
             ('%':'f':cs) -> pre ++ qfpath ++ walk False cs
             (c:cs)       -> pre ++ [c] ++ walk add cs
             []           -> pre ++ (if add then " " ++ qfpath else "")


{--------------------------------------------------------------------------
  Messages
--------------------------------------------------------------------------}

getCommand :: State -> ReadLineT IO Command
getCommand st
  = do let ansiPrompt = (if isAnsiPrinter (printer st)
                          then ansiWithColor (colorInterpreter (colorSchemeFromFlags (flags st)))
                          else id) "> "
       mbInput <- readLineEx ansiPrompt (prompt st)
       let input = maybe ":quit" id mbInput
       -- messageInfoLn st ("cmd: " ++ show input)
       let cmd   = parseCommand input
       return cmd

prompt ::  State -> IO ()
prompt st
  = do messageInfo st "> "
       flush (printer st)

remark ::  State -> String -> IO ()
remark st s
  = messageInfoLnLn st ("<" ++ s ++ ">")

terminal :: State -> Terminal
terminal st
  = Terminal (messageErrorMsgLn st) 
             (if (verbose (flags st) > 0) then (\s -> withColor (printer st) DarkGray (message st (s ++ "\n"))) else (\_ -> return ()))
             (messagePrettyLn st)  -- (\_ -> return ()) -- 
             (messageScheme st)
             (messagePrettyLn st)

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

messageMarker ::  State -> Range -> IO ()
messageMarker st rng
  = messagePrettyLn st makeMarker
  where
    colors
      = colorSchemeFromFlags (flags st)

    makeMarker
      = let c1 = posColumn (rangeStart rng)
        in color (colorMarker colors) (text (replicate (c1 + 1) ' ' ++ replicate 1 '^'))

{--------------------------------------------------------------------------
  Header
--------------------------------------------------------------------------}

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
    headerVersion = text $ "version " ++ version ++ (if buildVariant /= "release" then (" (" ++ buildVariant ++ ")") else "") ++ ", " ++ buildDate   

putQuote ::  State -> IO ()
putQuote st
  = do idx <- randomRIO (0,length quotes-1)
       let (quote,author) = quotes!!idx
       messageInfoLnLn st ("\n" ++ quote ++ "\n -- " ++ author)

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
