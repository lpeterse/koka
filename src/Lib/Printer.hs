------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module for portable control of colors in a console.
    Only the color of 'stdout' is influenced by these functions.
-}
-----------------------------------------------------------------------------
module Lib.Printer( 
                -- * Color
                Color(..)
                -- * Printer
                , Printer( write, writeText, writeLn, writeTextLn, flush, withColor, withBackColor, withReverse, withUnderline    , setColor, setBackColor, setReverse, setUnderline ) 
                -- * Printers
              , MonoPrinter, withMonoPrinter
              , AnsiPrinter, withAnsiPrinter
              , ConsolePrinter, withConsolePrinter
              , withFilePrinter, withNewFilePrinter
              , withHtmlPrinter, withHtmlPrinter
                -- * Misc.
              , ansiWithColor
                -- * Printer Monad
              , PrinterM(..)
              ) where

import Data.List( intersperse )
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
-- import Data.Char( toLower )
import System.IO  ( hFlush, stdout, hPutStr, hPutStrLn, openFile, IOMode(..), hClose, Handle )  
import Platform.Runtime( finally )
import Platform.Config( exeExtension )
import qualified Platform.Console as Con

import Data.Monoid (mappend, mconcat)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Monad.Reader
import Control.Monad.IO.Class

import Debug.Trace

{--------------------------------------------------------------------------
  Printer
--------------------------------------------------------------------------}  
-- | A printer is an abstraction for something where we can send
-- character output to.
class Printer p where
  write           :: p -> String -> IO ()
  writeText       :: p -> T.Text -> IO ()
  writeText    p t = write p (T.unpack t)
  writeLn         :: p -> String -> IO ()
  writeTextLn     :: p -> T.Text -> IO ()
  writeTextLn  p t = writeLn p (T.unpack t)
  flush           :: p -> IO ()
  withColor       :: p -> Color -> IO a -> IO a
  withBackColor   :: p -> Color -> IO a -> IO a
  withReverse     :: p -> Bool -> IO a -> IO a
  withUnderline   :: p -> Bool -> IO a -> IO a
  setColor        :: p -> Color -> IO ()
  setBackColor    :: p -> Color -> IO ()
  setReverse      :: p -> Bool -> IO ()
  setUnderline    :: p -> Bool -> IO ()

class PrinterM m where
  pWrite           :: String -> m ()
  pWriteText       :: T.Text -> m ()
  pWriteLn         :: String -> m ()
  pWriteTextLn     :: T.Text -> m ()
  pFlush           :: m ()
  pWithColor       :: Color  -> IO a -> m a
  pWithBackColor   :: Color  -> IO a -> m a
  pWithReverse     :: Bool   -> IO a -> m a
  pWithUnderline   :: Bool   -> IO a -> m a
  pSetColor        :: Color  -> m ()
  pSetBackColor    :: Color  -> m ()
  pSetReverse      :: Bool   -> m ()
  pSetUnderline    :: Bool   -> m ()

instance (Printer p, MonadIO m) => PrinterM (ReaderT p m) where
  pWrite           x = ask >>= \p-> liftIO (write p x)
  pWriteText       x = ask >>= \p-> liftIO (writeText p x)
  pWriteLn         x = ask >>= \p-> liftIO (writeLn p x)
  pWriteTextLn     x = ask >>= \p-> liftIO (writeTextLn p x)
  pFlush             = ask >>= \p-> liftIO (flush p)
  pWithColor     x m = ask >>= \p-> liftIO (withColor p x m)
  pWithBackColor x m = ask >>= \p-> liftIO (withBackColor p x m)
  pWithReverse   x m = ask >>= \p-> liftIO (withReverse p x m)
  pWithUnderline x m = ask >>= \p-> liftIO (withUnderline p x m)
  pSetColor        x = ask >>= \p-> liftIO (setColor p x)
  pSetBackColor    x = ask >>= \p-> liftIO (setBackColor p x)
  pSetReverse      x = ask >>= \p-> liftIO (setReverse p x)
  pSetUnderline    x = ask >>= \p-> liftIO (setUnderline p x)

{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}  
-- | Available colors on a console. Normally, background colors are
-- converted to their /dark/ variant.
data Color  = Black
            | DarkRed
            | DarkGreen
            | DarkYellow
            | DarkBlue
            | DarkMagenta
            | DarkCyan
            | Gray
            | DarkGray
            | Red
            | Green
            | Yellow
            | Blue
            | Magenta
            | Cyan
            | White
            | ColorDefault
            deriving (Show,Eq,Ord,Enum)

{--------------------------------------------------------------------------
  Simple monochrome printer
--------------------------------------------------------------------------}  

-- | On windows, we cannot print unicode characters :-(
sanitize :: String -> String
sanitize s | null exeExtension = s
sanitize s
  = map (\c -> if (c > '~') then '?' else c) s

sanitizeT :: T.Text -> T.Text
sanitizeT s | null exeExtension = s
sanitizeT s
  = T.map (\c -> if (c > '~') then '?' else c) s


-- | Monochrome console
newtype MonoPrinter  = MonoPrinter ()

-- | Use a black and white printer that ignores colors.
withMonoPrinter :: (MonoPrinter -> IO a) -> IO a
withMonoPrinter f
  = f (MonoPrinter ())

instance Printer MonoPrinter where
  write p s             = putStr $ sanitize s
  writeText p s         = T.putStr $ sanitizeT s
  writeLn p s           = putStrLn $ sanitize s
  writeTextLn p s       = T.putStrLn $ sanitizeT s
  flush p               = hFlush stdout
  withColor p c io      = io
  withBackColor p c io  = io
  withReverse p r io    = io
  withUnderline p u io  = io
  setColor p c          = return ()
  setBackColor p c      = return ()
  setReverse p r        = return ()
  setUnderline p u      = return ()


{--------------------------------------------------------------------------
  Simple file printer
--------------------------------------------------------------------------}  
-- | File printer
newtype FilePrinter = FilePrinter Handle

-- | Use a file printer that ignores colors. Appends, or creates the file
withFilePrinter :: FilePath -> (FilePrinter -> IO a) -> IO a
withFilePrinter fname f
  = do h <- openFile fname AppendMode
       x <- f (FilePrinter h)
       hFlush h
       hClose h
       return x

-- | Use a file printer that ignores colors. Creates or overwrites the file
withNewFilePrinter :: FilePath -> (FilePrinter -> IO a) -> IO a
withNewFilePrinter fname f
  = do h <- openFile fname WriteMode
       x <- f (FilePrinter h)
       hFlush h
       hClose h
       return x

instance Printer FilePrinter where
  write       (FilePrinter h) s = hPutStr h s
  writeText   (FilePrinter h) s = T.hPutStr h s
  writeLn     (FilePrinter h) s  = hPutStrLn h s
  writeTextLn (FilePrinter h) s  = T.hPutStrLn h s
  flush (FilePrinter h)      = hFlush h
  withColor p c io           = io
  withBackColor p c io       = io
  withReverse p r io         = io
  withUnderline p u io       = io
  setColor p c               = return ()
  setBackColor p c           = return ()
  setReverse p r             = return ()
  setUnderline p u           = return ()




{--------------------------------------------------------------------------
  Standard ANSI escape sequences
--------------------------------------------------------------------------}  
-- | Use a color printer that uses ANSI escape sequences.
withAnsiPrinter :: (AnsiPrinter -> IO a) -> IO a
withAnsiPrinter f
  = do ansi <- newIORef ansiDefault
       finally (f (Ansi ansi)) (do ansiEscapeIO seqReset
                                   hFlush stdout)

ansiDefault 
  = AnsiConsole ColorDefault ColorDefault False False


-- | Standard ANSI console
-- FIXME: refactor the IORef away
newtype AnsiPrinter = Ansi (IORef AnsiConsole)
data AnsiConsole = AnsiConsole{ fcolor    :: Color
                              , bcolor    :: Color
                              , invert    :: Bool
                              , underline :: Bool
                              }

instance Printer AnsiPrinter where
  write p s             = putStr s
  writeText p s         = T.putStr s
  writeLn p s           = putStrLn s
  writeTextLn p s       = T.putStrLn s
  flush p               = hFlush stdout
  withColor p c io      = ansiWithConsole p (\con -> con{ fcolor = c }) io
  withBackColor p c io  = ansiWithConsole p (\con -> con{ bcolor = c }) io
  withReverse p r io    = ansiWithConsole p (\con -> con{ invert = r }) io
  withUnderline p u io  = ansiWithConsole p (\con -> con{ underline = u }) io
  setColor p c          = unit $ ansiSetConsole p (\con -> con{ fcolor = c })
  setBackColor p c      = unit $ ansiSetConsole p (\con -> con{ bcolor = c })
  setReverse p r        = unit $ ansiSetConsole p (\con -> con{ invert = r })
  setUnderline p u      = unit $ ansiSetConsole p (\con -> con{ underline = u })


-- | Helper function to put a string into a certain color
ansiWithColor :: Color -> String -> String
ansiWithColor color s
  = let con0 = ansiDefault
        con1 = con0{ fcolor = color }
        pre  = ansiEscape (seqSetConsole con0 con1)
        post = ansiEscape (seqSetConsole con1 con0)
    in T.unpack $ pre `mappend` (T.pack s `mappend` post)

-- | Enable console color code.
unit io
  = io >> return ()

-- Console code
ansiWithConsole :: AnsiPrinter -> (AnsiConsole -> AnsiConsole) -> IO a -> IO a
ansiWithConsole p f io 
  = do old <- ansiSetConsole p f
       finally io (ansiSetConsole p (const old))

ansiSetConsole :: AnsiPrinter -> (AnsiConsole -> AnsiConsole) -> IO AnsiConsole
ansiSetConsole (Ansi varAnsi) f
  = do con <- readIORef varAnsi
       let new = f con
       ansiEscapeIO (seqSetConsole con new)
       writeIORef varAnsi new
       return con

ansiEscapeIO :: [T.Text] -> IO ()
ansiEscapeIO xs
  | null xs   = return ()
  | otherwise = T.putStr (ansiEscape xs) 
  
ansiEscape :: [T.Text] -> T.Text
ansiEscape xs
  | null xs   = T.empty
  | otherwise = T.pack "\ESC[" `mappend` ((mconcat $ intersperse (T.pack ";") xs) `mappend` T.pack "m")

seqSetConsole old new
  -- reset when any attributes are disabled
  | invert old > invert new               = reset
  | underline old > underline new         = reset
  | bold (fcolor old) > bold (fcolor new) = reset
  -- no attributes are disabled, we take a diff
  | otherwise = diff
  where
    reset = concat
            [seqReset
            ,seqReverse (invert new) 
            ,seqUnderline (underline new) 
            ,seqBold (bold (fcolor new))
            ,seqColor False (fcolor new)
            ,seqColor True (bcolor new)]

    diff  = concat 
            [max seqReverse invert
            ,max seqUnderline underline
            ,max seqBold (bold . fcolor)
            ,max (seqColor False) fcolor
            ,max (seqColor True) bcolor
            ]

    max f field 
      = if (field old /= field new) then f (field new) else []

seqReset :: [T.Text]
seqReset
  = [T.pack "00"]

seqUnderline :: Bool -> [T.Text]
seqUnderline u
  = if u then [T.pack "04"] else []

seqReverse rev
  = if rev then [T.pack "07"] else []

seqBold b
  = if b then [T.pack "01"] else []

bold c
  = case c of
      ColorDefault -> False
      _            -> (fromEnum c >= 8) 

seqColor :: Bool -> Color -> [T.Text]
seqColor backGround c
  = case c of
      ColorDefault 
        -> encode 9
      _ -> encode (fromEnum c `mod` 8)
  where
    encode i
      = [T.pack $ show (i + if backGround then 40 else 30)]


{--------------------------------------------------------------------------
  Windows console code
--------------------------------------------------------------------------}  
-- | Windows console printer
newtype ConsolePrinter = ConsolePrinter ()

instance Printer ConsolePrinter where
  write p s             = putStr $ sanitize s
  writeText p s         = T.putStr $ sanitizeT s
  writeLn p s           = putStrLn $ sanitize s
  writeTextLn p s       = T.putStrLn $ sanitizeT s
  flush p               = hFlush stdout
  withColor p c io      = Con.bracketConsole (do Con.setColor c; io)
  withBackColor p c io  = Con.bracketConsole (do Con.setBackColor c; io)
  withReverse p r io    = Con.bracketConsole (do Con.setReverse r; io) 
  withUnderline p u io  = Con.bracketConsole (do Con.setUnderline u; io)
  setColor p c          = Con.setColor c
  setBackColor p c      = Con.setBackColor c
  setReverse p r        = Con.setReverse r
  setUnderline p u      = Con.setUnderline u

withConsolePrinter :: (ConsolePrinter -> IO a) -> IO a
withConsolePrinter f
  = f (ConsolePrinter ())

{--------------------------------------------------------------------------
  HTML printer
--------------------------------------------------------------------------}  
data HtmlPrinter = HtmlPrinter ()

withHtmlPrinter :: (HtmlPrinter -> IO a) -> IO a
withHtmlPrinter f
  = f (HtmlPrinter ())

instance Printer HtmlPrinter where
  write p s             = putStr (htmlEscape s)
  writeText p s         = write p (T.unpack s)
  writeLn p s           = putStrLn (htmlEscape s)
  writeTextLn p s       = writeLn p (T.unpack s)
  flush p               = hFlush stdout
  withColor p c io      = htmlSpan (T.pack "color") (htmlColor c) io
  withBackColor p c io  = htmlSpan (T.pack "background-color") (htmlColor c) io
  withReverse p r io    = {- no supported -} io
  withUnderline p u io  = htmlSpan (T.pack "text-decoration") (T.pack "underline") io
  setColor p c          = return ()
  setBackColor p c      = return ()
  setReverse p r        = return ()
  setUnderline p u      = return ()

htmlSpan :: T.Text -> T.Text -> IO a -> IO a
htmlSpan prop val io
  = do T.putStr $ T.pack "<span style='"
       T.putStr $ prop
       T.putStr $ T.pack ": "
       T.putStr $ val
       T.putStr $ T.pack "'>"
       x <- io
       T.putStr $ T.pack "</span>"
       return x

htmlColor :: Color -> T.Text
htmlColor c
  = case c of 
      ColorDefault  -> T.pack "black"
      _             -> T.toLower (T.pack $ show c)

htmlEscape s
  = concatMap escape s
  where
    escape c
      = case c of
          '&' -> "&amp;"
          '<' -> "&lt;"
          '>' -> "&gt;"
          '"' -> "&quot;"
          '\'' -> "&apos;"
          _   -> [c]
