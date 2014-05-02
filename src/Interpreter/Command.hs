{-# OPTIONS -fspec-constr-count=0 #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

{- |
  Command parsing and interpretation.
-}

module Interpreter.Command
  ( Command(..)
  , ShowCommand(..)
  , parseCommand
  , commandHelp
  ) where

import Data.Char          ( isSpace, toLower )
import Data.List          ( isPrefixOf )
import Text.Parsec hiding (Error)

import Common.ColorScheme
import Lib.PPrint         ( Doc, text,vcat,(<$>),hang,empty,fill,(<>),color)

type Parser a = Parsec String () a

{--------------------------------------------------------------------------
  Interpreter Command parsing
--------------------------------------------------------------------------}
-- | A commmand line command.
data Command  = Quit
              | Error    String
              | Load     [FilePath]
              | Reload 
              | Eval     String
              | TypeOf   String
              | KindOf   String
              | Define   String
              | TypeDef  String
              | Options  String
              | Edit     FilePath
              | Shell    String
              | ChangeDir FilePath
              | Show     ShowCommand
              | None
              
-- | A /show/ command -- @?@.
data ShowCommand
              = ShowSource
              | ShowTypeSigs
              | ShowKindSigs
              | ShowSynonyms
              | ShowDefines
              | ShowHelp
              | ShowVersion

{--------------------------------------------------------------------------
  Parse a command line into a command, raises an exception on failure.
--------------------------------------------------------------------------}

-- | Parse a command line into a command, uses constructor 'Error' on failure.
parseCommand :: String -> Command
parseCommand input
  = case parse (wrap command) "" input of
      Left err  -> Error ("error in command: " ++ show err)
      Right cmd -> cmd

-- | Dispatch between expressions and commands preceeded by a colon.
command :: Parser Command
command 
  =   do{ special ":"; cmdeval }
  <|> do{ expression }
  <|> return None
  <?> "command"

-- | Parse a command without the preceeding colon.
cmdeval :: Parser Command
cmdeval
  =   do{ symbol "l" <|> symbol "load"; fpaths <- filenames; return (Load fpaths) }
  <|> do{ symbol "r" <|> symbol "reload"; return Reload }
  <|> do{ symbol "q" <|> symbol "quit"; return Quit }
  -- external
  <|> do{ symbol "e" <|> symbol "edit"; fpath <- option "" filename; return (Edit fpath) }
  <|> do{ symbol "cd"; fpath <- option "" filename; return (ChangeDir fpath) }
  <|> do{ special "!"; cmd <- shellCommand; return (Shell cmd) }
  -- help
  <|> do{ special "?" <|> symbol "h" <|> symbol "help"; return (Show ShowHelp) }
  -- complex
  <|> do{ symbol "t" <|> symbol "type" <|> symbol "b" <|> symbol "browse"; 
        (do p <- getPosition; x <- expr; return (TypeOf (replicate (sourceColumn p-1) ' ' ++ x))
         <|>
         return (Show ShowTypeSigs))}
  <|> do{ symbol "set"; opts <- commandLine; return (Options opts) }
  <|> do{ symbol "s" <|> symbol "source"
        ; return (Show (ShowSource))}
  <|> do{ symbol "k" <|> symbol "kind";   
          (do p <- getPosition; x <- expr; return (KindOf (replicate (sourceColumn p-1) ' ' ++ x))
           <|>
           return (Show ShowKindSigs))}
  <|> do{ symbol "d" <|> symbol "defines"
        ; return (Show ShowDefines) }
  <|> do{ symbol "alias"; return (Show ShowSynonyms) }
  <|> do{ symbol "w" <|> symbol "warranty" <|> symbol "version"; return (Show ShowVersion) }
  <?> "command"

-- | The interpreter help.
commandHelp :: ColorScheme -> Doc
commandHelp colors
  = hang 2 (infotext "commands:" <$> vcat 
    [cmd "<expression>" ""          "evaluate the given expression"
    ,cmd "val"      "<definition>"  "add a value definition"
    ,cmd "fun"      "<definition>"  "add a function definition"
    ,cmd "type"     "<definition>"  "add a new type definition"
    ,cmd "alias"    "<definition>"  "add a type synonym definition"
    ,empty
    ,cmd ":l[oad]"  "{modulename}"  "load module(s)"
    ,cmd ":r[eload]" ""             "reload the current module(s)"
--    ,cmd ":f[ind]" "<identifier>"   "edit file containing the identifier"
    ,cmd ":e[dit]" "[filename]"     "edit file (and jump to error location)"   
    ,cmd ":set"    "<options>"      "set (command line) options"
    ,empty
    ,cmd ":t[ype]" "[expression]"   "show type signature(s) (of a given expression)"
    ,cmd ":k[ind]" "[type]"         "show kind signature(s) (of a given type)"
    ,cmd ":s[ource]" ""             "show the source code of the current module"
    ,cmd ":d[efines]" "[identifier]" "show interactively defined values"
    ,cmd ":alias"   ""            "show type alias signatures"
    ,cmd ":version"  ""             "show version and warranty information"
    ,cmd ":cd"     ""               "show the current directory"
    ,cmd ":cd"     "<directory>"    "change the current directory"
    ,cmd ":!"      "<command>"      "run a shell command"
    ,cmd ":?"      ""               "show this information"
    ,cmd ":q[uit]"  ""              "quit the interpreter"
    ,empty
    ]) <$>
    hang 2 (infotext "remarks:" <$> vcat
    [text "The type command can also be cotype, rectype, or struct."
    ,text "Use :set -? to see help on command line flags."
    ]) 
  where
    cmd c arg explain
      = fill 12 (text c) <> fill 14 (text arg) <> infotext explain
    infotext s
      = color (colorInterpreter colors) (text s)

-- | Parse definition commands. 
expression :: Parser Command
expression
  = do src <- expr
       return (f src)
  where
    f src | isPrefixOf "fun"     src = Define src
          | isPrefixOf "val"     src = Define src
          | isPrefixOf "type"    src = TypeDef src
          | isPrefixOf "cotype"  src = TypeDef src
          | isPrefixOf "rectype" src = TypeDef src
          | isPrefixOf "alias"   src = TypeDef src
          | isPrefixOf "struct"  src = TypeDef src
          | isPrefixOf "enum"    src = TypeDef src
          | otherwise                = Eval src

expr :: Parser String
expr
  = anything <?> "expression"

shellCommand :: Parser String
shellCommand
  = anything <?> "shell command"

commandLine :: Parser String
commandLine
  = many (noneOf "\n") <?> "options"

anything :: Parser String
anything
  = lexeme (many1 (noneOf "\^Z"))

filenames :: Parser [String]
filenames
  = many filename

filename :: Parser String
filename
  = lexeme (
    do{ _ <- char '"'
      ; s <- many1 (noneOf "\"\n")
      ; _ <- char '"'
      ; return s
      }
    <|>
    do{ many1 (noneOf "\"\n \t")  }
    <?> "file name")

{--------------------------------------------------------------------------
  Whitespace and lexemes
--------------------------------------------------------------------------}    

special :: String -> Parser ()
special name
  = lexeme (do{ _ <- string name; return () })

symbol :: String -> Parser ()
symbol name
  = lexeme (try (do{ _ <- istring name; notFollowedBy alphaNum }))

istring :: String -> Parser ()
istring s
  = (mapM_ (\c -> satisfy (\d -> toLower d == toLower c)) s)
    <?> s

lexeme :: Parser a -> Parser a
lexeme p       
    = do{ x <- p; whiteSpace; return x  }

wrap :: Parser a -> Parser a
wrap p
  = do{ whiteSpace
      ; x <- p
      ; eof
      ; return x
      }

whiteSpace :: Parser ()
whiteSpace 
  = skipMany white

white :: Parser ()
white
  = simpleSpace <|> {- oneLineComment <|> -} multiLineComment <?> ""

simpleSpace :: Parser ()
simpleSpace
  = skipMany1 (satisfy isSpace)

multiLineComment :: Parser ()
multiLineComment
  = do { _ <- try (string "{-")
       ; inComment
       }

inComment :: Parser ()
inComment
    =   do{ _ <- try (string "-}") ; return () }
    <|> do{ multiLineComment             ; inComment}
    <|> do{ skipMany1 (noneOf startEnd)  ; inComment}
    <|> do{ _ <- oneOf startEnd               ; inComment}
    <?> "end of comment"  
    where
      startEnd   = "{-}"

