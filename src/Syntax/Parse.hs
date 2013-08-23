------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Parse concrete syntax.
-}
-----------------------------------------------------------------------------
module Syntax.Parse( parseProgramFromFile
                   , parseValueDef
                   , parseTypeDef
                   , parseExpression
                   , parseType

                   -- used by the core parser
                   , lexParse, parseLex, LexParser
                  
                   , visibility, modulepath, importAlias
                   , tbinderId, constructorId, funid, paramid
                   , braced, semiBraces, semis, semiColons
                   , angles, anglesCommas, parensCommas, parens
                   , semiColon, lparen, rparen, langle, rangle, comma
                   , qtypeid, qvarid, qconid, qidop, identifier, qoperator, varid
                   , integer, charLit, floatLit, stringLit
                   , special, specialId, specialOp, specialConId
                   , keyword, dockeyword
                   ) where

-- import Lib.Trace
import Data.List (intersperse)
import Lib.PPrint hiding (string,parens,integer,semiBraces,lparen,comma,angles,rparen,rangle,langle)
import qualified Lib.PPrint as PP (string)

import Control.Monad (mzero)
import Text.Parsec hiding (space,tab,lower,upper,alphaNum,sourceName)
import Text.Parsec.Error
import Text.Parsec.Pos           (newPos)

import Common.Name
import Common.NamePrim
import Common.Range hiding (after)
import Common.File
import Platform.Config 
import Platform.Runtime( unsafePerformIO, exCatch )
import Common.Error
import Common.Syntax

import Syntax.Syntax
import Syntax.Lexeme
import Syntax.Lexer   ( lexing )
import Syntax.Layout  ( layout )
import Syntax.Promote ( promote, promoteType, quantify )


-----------------------------------------------------------
-- Parser on token stream
-----------------------------------------------------------

type LexParser a  = Parsec [Lexeme] () a -- GenParser Lexeme () a

parseLex :: Lex -> LexParser Lexeme
parseLex lex
  = token showTok posFromTok testTok
  where
    showTok (Lexeme _ lex)       = show lex
    posFromTok (Lexeme range _)  = newPos "" (posLine (rangeStart range)) (posColumn (rangeStart range))
    testTok l@(Lexeme _ lex')    | sameLex lex lex'  = Just l
                                 | otherwise         = Nothing

-----------------------------------------------------------
-- Parse varieties
-----------------------------------------------------------
parseProgramFromFile :: Bool -> FilePath -> IO (Error UserProgram)
parseProgramFromFile semiInsert fname
  = do input <- readInput fname
       return (lexParse semiInsert program fname 1 input)


parseValueDef :: Bool -> FilePath -> Int -> String -> Error UserDef
parseValueDef semiInsert sourceName line input
  = lexParseS semiInsert (const valueDefinition)  sourceName line input 

parseTypeDef :: Bool -> FilePath -> Int -> String -> Error (UserTypeDef,[UserDef])
parseTypeDef semiInsert sourceName line input
  = lexParseS semiInsert (const typeDefinition)  sourceName line input 

parseType :: Bool -> FilePath -> Int -> Name -> String -> Error UserTypeDef
parseType semiInsert sourceName line name input
  = lexParseS semiInsert (const (userType name))  sourceName line input

parseExpression :: Bool -> FilePath -> Int -> Name -> String -> Error UserDef
parseExpression semiInsert sourceName line name input
  = lexParseS semiInsert (const (expression name))  sourceName line input

lexParseS semiInsert p sourceName line str
  = lexParse semiInsert p sourceName line (stringToBString str)

lexParse :: Bool -> (Source -> LexParser a) -> FilePath -> Int -> BString -> Error a
lexParse semiInsert p sourceName line rawinput
  = let source = Source sourceName rawinput 
        input  = if (extname sourceName == sourceExtension ++ "doc") then extractLiterate rawinput else rawinput
        xs = lexing source line input 
        lexemes = layout semiInsert xs
    in  -- trace  (unlines (map show lexemes)) $
        case (parse (p source) sourceName lexemes) of
          Left err -> makeParseError (errorRangeLexeme xs source) err
          Right x  -> return x


makeParseError :: (ParseError -> Range) -> ParseError -> Error a
makeParseError toRange perr
  = errorMsg (ErrorParse (toRange perr) errorDoc)
  where
    errorDoc 
      = PP.string ("invalid syntax" ++ (drop 1 $ dropWhile (/=':') $ show perr))


errorRangeLexeme :: [Lexeme] -> Source -> ParseError -> Range
errorRangeLexeme lexemes source perr
  = case dropWhile (\r -> r < range) (map getRange lexemes) of
      (lrange : _) | rangeStart lrange == rangeStart range  -> lrange
      _            -> range
  where
    range = makeRange pos pos
    pos = makePos source (-1) (sourceLine (errorPos perr)) (sourceColumn (errorPos perr))


{--------------------------------------------------------------------------
  Interactive
--------------------------------------------------------------------------}
interactive :: LexParser a -> LexParser a
interactive p
  = do x <- p
       many semiColon  
       eof
       return x

valueDefinition :: LexParser UserDef
valueDefinition
  = interactive (pureDecl Private)


typeDefinition :: LexParser (UserTypeDef,[UserDef])
typeDefinition
  = interactive (do tdef <- aliasDecl Public 
                    return (tdef,[])
                 <|> 
                 typeDecl Public)

expression :: Name -> LexParser (UserDef) -- ,UserDef)
expression name
  = interactive $
    do e <- aexpr
       let r = getRange e
       return (Def (ValueBinder name () (Lam [] e r) r r)  r Public DefFun ""
              -- ,Def (ValueBinder (prepend ".eval" name) () (Lam [] (App (Var nameGPrint False r) [Var name False r] r)))
              )

userType :: Name -> LexParser UserTypeDef
userType name
  = interactive $
    do tp <- ptype 
       let rng = getRange tp
       return (Synonym (TypeBinder name KindNone rangeNull rangeNull) [] tp rng Public "")
       

-----------------------------------------------------------
-- Program
-----------------------------------------------------------
program :: Source -> LexParser UserProgram
program source
  = do many semiColon
       p <- pmodule source
       eof
       return p


pmodule :: Source -> LexParser UserProgram
pmodule source
  = do (vis,rng,doc) <- try $ do (vis,_) <- visibility Private 
                                 (rng,doc) <- dockeyword "module"
                                 return (vis,rng,doc)
       -- (rng,doc) <- dockeyword "module"
       (name,rng) <- modulepath
       programBody vis source name rng doc
  <|>
    programBody Public source (newName (basename (sourceName source))) (rangeNull) ""

programBody vis source modName nameRange doc
  = do many semiColon
       (imports, fixDefss, topDefss)
          <- braced (do imps <- semis importDecl
                        fixs <- semis fixDecl
                        tdefs <- semis (topdef vis)
                        return (imps,fixs,tdefs))
       many semiColon          
       let (defs,typeDefs,externals) = splitTopDefs (concat topDefss)
       return (Program source modName nameRange [TypeDefRec typeDefs] [DefRec defs] (prelude ++ imports) externals (concat fixDefss) doc)
  where
    prelude = if (modName == nameSystemCore)
               then []
               else [Import nameSystemCore nameSystemCore rangeNull Private]

braced p
  = do lcurly
       many semiColon
       x <- p
       rcurly
       return x
  <|> 
    do parseLex LexInsLCurly
       many semiColon
       x <- p
       many semiColon
       parseLex LexInsRCurly
       return x
  <|>
    p 

-- collect definitions
data TopDef
  = DefValue   UserDef 
  | DefType    UserTypeDef
  | DefExtern  External

splitTopDefs ds
  = fold ([],[],[]) ds
  where
    fold (defs,tdefs,edefs) []  = (reverse defs, reverse tdefs, reverse edefs)
    fold (defs,tdefs,edefs) (d:ds)
      = case d of
          DefValue def  -> fold (def:defs,  tdefs, edefs) ds
          DefType tdef  -> fold (defs, tdef:tdefs, edefs) ds
          DefExtern edef-> fold (defs, tdefs, edef:edefs) ds


topdef :: Visibility -> LexParser [TopDef]
topdef vis
  = do def <- pureDecl vis
       return [DefValue def]
  <|>
    do tdef <- aliasDecl vis 
       return [DefType tdef]
  <|> 
    do (tdef,cdefs) <- typeDecl vis
       return ([DefType tdef] ++ map DefValue cdefs)
  <|> 
    do externDecl vis


{---------------------------------------------------------------
  Import declaration
---------------------------------------------------------------}
importDecl :: LexParser Import
importDecl 
  = do (vis,vrng,rng0) <- try $ do (vis,vrng) <- visibility Private
                                   rng0  <- keyword "import"
                                   return (vis,vrng,rng0)
       (asname,name,rng) <- importAlias  
       return (Import asname name (combineRanges [vrng,rng0,rng]) vis)

importAlias :: LexParser (Name,Name,Range)
importAlias
  = do (name1,rng1) <- modulepath
       (do keyword "="
           (name2,rng2) <- modulepath
           return (name1,name2,rng2)
        <|> return (name1,name1,rng1))  
       

     
visibility :: Visibility -> LexParser (Visibility,Range)
visibility vis
  =   do rng <- keyword "public"; return (Public,rng)
  <|> do rng <- keyword "private"; return (Private,rng)
  <|> return (vis,rangeNull)



{--------------------------------------------------------------------------
  External
--------------------------------------------------------------------------}
externDecl :: Visibility -> LexParser [TopDef]
externDecl dvis
  = do (vis,vrng)  <- visibility dvis
       (krng,doc) <- dockeyword "external"
       (do specialId "include"
           extern <- externalInclude (combineRange vrng krng)
           return [DefExtern extern]
        <|>
        do keyword "import"
           extern <- externalImport (combineRange vrng krng)
           return [DefExtern extern]
        <|>
        do isInline <- do{ specialId "inline"; return True } <|> return False
           (name,nameRng) <- funid
           (pars,args,tp,annotate)
             <- do keyword ":"
                   tp <- ptype  -- no "some" allowed
                   (pars,args) <- genParArgs (promoteType tp)
                   return (pars,args,tp,\body -> Ann body tp (getRange tp)) 
                <|> 
                do tpars <- typeparams
                   (pars,parRng) <- parameters (not isInline) {- allow defaults? -}
                   (teff,tres)   <- annotResult
                   let tp = typeFromPars nameRng pars teff tres
                   genParArgs tp -- checks the type
                   return (pars,genArgs pars,tp,\body -> promote [] tpars (Just (Just teff, tres)) body)
           (exprs,rng) <- externalBody
           if (isInline)
            then return [DefExtern (External name tp nameRng (combineRanges [vrng,krng,rng]) exprs vis doc)]
            else do let  externName = newHiddenExternalName name
                         fullRng    = combineRanges [vrng,krng,rng]
                         extern     = External externName tp (before nameRng) (before fullRng) exprs Private doc
                         
                         body       = annotate (Lam pars (App (Var externName False rangeNull) args fullRng) fullRng)
                         binder     = ValueBinder name () body nameRng fullRng
                         extfun     = Def binder fullRng vis DefFun doc
                    return [DefExtern extern, DefValue extfun]
        )
  where
    typeFromPars :: Range -> [ValueBinder (Maybe UserType) (Maybe UserExpr)] -> UserType -> UserType -> UserType
    typeFromPars rng pars teff tres
      = promoteType $ TpFun [(binderName p, tp) | p <- pars, let Just tp = binderType p] teff tres rng

    genArgs pars
      = [(Nothing,Var (binderName p) False (before (getRange p))) | p <- pars]  

    genParArgs tp
      = case tp of
          TpQuan QSome _ _ _ -> fail "external types cannot contain unspecified ('_') types"
          TpQuan QExists _ _ _ -> fail "external types cannot contain existential types"
          TpQuan _ _ t _ -> genParArgs t
          TpQual _ t     -> genParArgs t
          TpParens t _   -> genParArgs t
          TpAnn t _      -> genParArgs t
          TpFun pars _ _ _ -> return $ genFunParArgs pars
          _                -> fail "external declarations must have a function type"
          
    genFunParArgs pars
      = unzip (map genParArg (zip pars [1..]))

    genParArg ((name,tp),idx)
      = let fullName = if name == nameNil then newHiddenName ("arg" ++ show idx) else name
            rng = rangeNull -- before (getRange tp)
        in (ValueBinder fullName Nothing Nothing rng rng
           ,(Nothing,Var fullName False rng))

externalImport :: Range -> LexParser External
externalImport rng1
  = do keyword "="
       (entry) <- externalImportEntry
       return (ExternalImport [entry] rng1)
  <|>
    do (entries,rng2) <- semiBracesRanged externalImportEntry 
       return (ExternalImport entries (combineRange rng1 rng2))
  where
    externalImportEntry
      = do target <- externalTarget
           (s,rng) <- stringLit
           return (target,s)


externalInclude :: Range -> LexParser External
externalInclude rng1
  = do keyword "="
       (entry) <- externalIncludeEntry
       return (ExternalInclude [entry] rng1)
  <|>
    do (entries,rng2) <- semiBracesRanged externalIncludeEntry 
       return (ExternalInclude entries (combineRange rng1 rng2))
       
externalIncludeEntry
  = do target <- externalTarget
       (do specialId "file"
           (fname,rng) <- stringLit
           content <- preadFile fname (Common.Range.sourceName (rangeSource rng))
           return (target,content)
        <|>
        do (s,rng) <- stringLit
           return (target,s)
        )
  where 
    preadFile :: FilePath -> FilePath -> LexParser String
    preadFile fname currentFile
      = do pos <- getPosition
           let fpath      = if (null (dirname fname)) then joinPath (dirname currentFile) fname else fname
               mbContent  = unsafePerformIO $ exCatch (do{ content <- readFile fpath; return (Just content) }) (\exn -> return Nothing)
           case mbContent of
             Just content -> return content
             Nothing      -> fail ("unable to read external file: " ++ fpath)
  

externalBody :: LexParser ([(Target,ExternalCall)],Range)
externalBody 
  = do keyword "="
       (target,inline,rng) <- externalEntryRanged
       return ([(target,inline)],rng)
  <|>
    do semiBracesRanged externalEntry

externalEntry
  = do (target,inline,_) <- externalEntryRanged
       return (target,inline)

externalEntryRanged
  = do target <- externalTarget
       (call,rng) <- externalCall
       return (target,call,rng)
                 
externalCall
  = do f <- do specialId "inline"
               return ExternalInline
            <|>
            do return ExternalCall
       (s,rng) <- stringLit 
       return (f s,rng)

                 
externalTarget
  = do specialId "cs"
       return CS
  <|>
    do specialId "js"
       return JS
  <|>
    return Default



{--------------------------------------------------------------------------
  Fixity declaration
--------------------------------------------------------------------------}
fixDecl :: LexParser FixDefs
fixDecl
  = do assoc <- assocDef
       (n,_) <- integer
       -- convenient to check here, but it really should be done during static analysis.
       if (n < 0 || n > 100)
        then fail "The precedence must be between 0 and 100" 
        else return ()
       let prec = fromInteger n
       names <- sepBy1 identifier comma
       return [FixDef name (FixInfix prec assoc) rng | (name,rng) <- names]
{-
  <|>
    do fix   <- do{ keyword "prefix"; return FixPrefix } 
                <|> 
                do{ keyword "postfix"; return FixPostfix } 
       names <- sepBy1 identifier comma
       return [FixDef name fix rng | (name,rng) <- names]
-}
assocDef
  =   do keyword "infixl"; return AssocLeft
  <|> do keyword "infixr"; return AssocRight
  <|> do keyword "infix"; return AssocNone

{--------------------------------------------------------------------------
  Type definitions
--------------------------------------------------------------------------}
aliasDecl :: Visibility -> LexParser UserTypeDef
aliasDecl dvis
  = do (vis,vrng,trng,doc) <- try$ do (vis,vrng) <- visibility dvis
                                      (trng,doc) <- dockeyword "alias"
                                      return (vis,vrng,trng,doc)                                 
       tbind <- tbinderDef
       (tpars,kind,krng) <- typeKindParams
       keyword "="
       tp <- ptype
       let range = combineRanges [vrng,trng,krng,getRange tp]
       return (Synonym (tbind kind) tpars tp range vis doc)

typeDecl :: Visibility -> LexParser (UserTypeDef, [UserDef])
typeDecl dvis
  = do (vis,defvis,vrng,(mbTypeSort,trng,doc)) <- 
          do rng <- keyword "abstract"
             x   <- typeDeclKind 
             return (Public,Private,rng,x)
          <|>
            (try $ do (vis,vrng) <- visibility dvis
                      x <- typeDeclKind 
                      return (vis,vis,vrng,x))

       case mbTypeSort of
         Just typeSort -> -- type, rectype or cotype                     
            do tbind <- tbinderDef
               (tpars,kind,prng) <- typeKindParams
               let name = tbind kind
                   resTp = TpApp (tpCon name) (map tpVar tpars) (combineRanged name tpars)
               (cs,crng)    <- semiBracesRanged (constructor defvis tpars resTp) <|> return ([],rangeNull)
               let (constrs,creatorss) = unzip cs
                   range   = combineRanges [vrng,trng, getRange (tbind kind),prng,crng]
               return (DataType name tpars constrs range vis typeSort doc, concat creatorss)
          
         Nothing ->  -- struct
            do tbind <- tbinderDef
               tpars <- angles tbinders <|> return []
               let name = tbind KindNone
                   resTp = TpApp (tpCon name) (map tpVar tpars) (combineRanged name tpars)

               (pars,prng)  <- conPars
               let (tid,rng) = getRName name
                   conId     = toConstructorName tid
                   (usercon,creators) = makeUserCon conId tpars resTp [] pars rng (combineRange rng prng) defvis doc
               return (DataType name tpars [usercon] (combineRanges [vrng,trng,rng,prng]) vis Inductive doc, creators)
         
  where
    tpVar tb = TpVar (tbinderName tb) (tbinderRange tb)
    tpCon tb = TpCon (tbinderName tb) (tbinderRange tb)
               
  {-
  <|>
    do trng <- keyword "enum"
       tbind <- tbinderDef
       es <- semiBraces enum <|> return []
       return (DataType (tbind KindNone) [] es (combineRanged trng es))

enum
  = do (con,rng) <- constructorId
       return (UserCon con [] [] rng rng)
  -}

typeDeclKind :: LexParser (Maybe DataKind,Range,String)
typeDeclKind
  = do (rng,doc) <- dockeyword "type"
       return (Just Inductive,rng,doc)
  <|> 
    do (rng,doc) <- dockeyword "cotype"
       return (Just CoInductive,rng,doc)
  <|> 
    do (rng,doc) <- dockeyword "rectype"
       return (Just Retractive, rng,doc)
  <|> 
    do (rng,doc) <- dockeyword "struct"
       return (Nothing, rng,doc)
  
typeKindParams
   = do (tpars,rng) <- anglesRanged tbinders
        kind  <- kindAnnot
        return (tpars,kind,combineRanged rng kind)
     <|>
     do kind <- kindAnnot
        return ([],kind,getRange kind)
     <|>
        return ([],KindNone,rangeNull)


constructor :: Visibility -> [UserTypeBinder] -> UserType -> LexParser (UserCon UserType UserType UserKind, [UserDef])
constructor defvis foralls resTp
  = do ((vis,vrng),(rng0,doc),(con,rng)) <- try $ do v <- visibility defvis
                                                     k <- dockeyword "con" <|> return (rangeNull,"")
                                                     c <- constructorId
                                                     return (v,k,c)
       exists    <- typeparams
       (pars,prng) <- conPars
       return (makeUserCon con foralls resTp exists pars rng (combineRanges [vrng,rng0,rng,getRange exists,prng]) vis doc)

makeUserCon :: Name -> [UserTypeBinder] -> UserType -> [UserTypeBinder] -> [ValueBinder UserType (Maybe UserExpr)] -> Range -> Range -> Visibility -> String -> (UserCon UserType UserType UserKind, [UserDef])
makeUserCon con foralls resTp exists pars nameRng rng vis doc
  = (UserCon con exists conParams nameRng rng vis doc
    ,if (any (isJust . binderExpr) pars) then [creator] else [])  
  where
    conParams
      = [par{ binderExpr = Nothing } | par <- pars]
    creator 
      = let name = newCreatorName con
            def  = Def binder rng vis DefFun doc
            binder    = ValueBinder name () body nameRng nameRng
            body      = Ann (Lam lparams (App (Var con False nameRng) arguments rng) rng) tpFull rng
            params    = [par{ binderType = (if (isJust (binderExpr par)) then makeOptional (binderType par) else binderType par) }  | par <- pars]
            lparams   = [par{ binderType = Nothing} | par <- params]
            arguments = [(Nothing,Var (binderName par) False (binderNameRange par)) | par <- params]
            tpParams  = [(binderName par, binderType par) | par <- params] 
            tpFull    = quantify QForall foralls (TpFun tpParams (makeTpTotal nameRng) resTp rng)
            makeOptional tp = TpApp (TpCon nameTpOptional (getRange tp)) [tp] (getRange tp)
        in def

    isJust (Just{}) = True
    isJust _        = False

conPars
  = parensCommasRng conBinder
  <|>
    return ([],rangeNull)

conBinder
  = do (name,rng,tp) <- paramType
       (opt,drng)    <- defaultExpr
       return (ValueBinder name tp opt rng (combineRanges [rng,getRange tp,drng]))
{-
    do (name,rng) <- try (do{ (Var name _ rng) <- variable; keyword ":"; return (name,rng) })
       tp <- ptype <?> "field type"
       return (ValueBinder name tp Nothing rng)
  <|>
    do tp <- ptype <?> "field type"
       return (ValueBinder nameNil tp Nothing (getRange tp))
-}
  <?>
    "constructor field"

constructorId
  = try ttuple
  <|>
    tlist
  <|> 
    conid
  <?> "constructor"


-----------------------------------------------------------
-- Value definitions
-----------------------------------------------------------

pureDecl :: Visibility -> LexParser UserDef
pureDecl dvis
  = do (vis,vrng,rng,doc,isVal) <- try $ do (vis,vrng) <- visibility dvis
                                            (do (rng,doc) <- (dockeyword "fun" <|> dockeyword "function"); return (vis,vrng,rng,doc,False)
                                             <|>
                                             do (rng,doc) <- dockeyword "val"; return (vis,vrng,rng,doc,True)) 
       (if isVal then valDecl else funDecl) (combineRange vrng rng) doc vis
       -- valueDecl vrng vis <|> functionDecl vrng vis
       
valueDecl vrng vis
  = do (rng,doc) <- dockeyword "val"
       valDecl (combineRange vrng rng) doc vis
  
functionDecl vrng vis
  = do (rng,doc) <- dockeyword "fun" <|> dockeyword "function"
       funDecl (combineRange vrng rng) doc vis

varDecl 
  = do (vrng,doc) <- dockeyword "var"
       bind <- binder
       keyword ":="
       body <- valexpr
       return (Def (bind body) (combineRanged vrng body) Private DefVar doc)

valDecl rng doc vis
  = do bind <- binder
       keyword "="
       body <- valexpr
       return (Def (bind body) (combineRanged rng body) vis DefVal doc)
 
funDecl rng doc vis
  = do spars <- squantifier
       -- tpars <- aquantifier  -- todo: store somewhere
       (name,nameRng) <- funid
       (tpars,pars,parsRng,mbtres,ann) <- funDef  
       body   <- block <|> do{ keyword "="; branchexpr }
       let fun = promote spars tpars mbtres
                  (Lam pars body (combineRanged rng body)) 
       return (Def (ValueBinder name () (ann fun) nameRng nameRng) (combineRanged rng fun) vis DefFun doc)

-- fundef: forall parameters, parameters, (effecttp, resulttp), annotation
funDef :: LexParser ([TypeBinder UserKind],[ValueBinder (Maybe UserType) (Maybe UserExpr)], Range, Maybe (Maybe UserType, UserType), UserExpr -> UserExpr)
funDef
  = do tpars  <- typeparams
       (pars,rng) <- parameters True
       resultTp <- annotRes
       -- todo: qualifiers
       return (tpars,pars,rng,resultTp,id)


annotRes :: LexParser (Maybe (Maybe UserType,UserType))
annotRes
  = do (teff,tres) <- annotResult
       return (Just (Just teff, tres))
  <|>
    return Nothing

annotResult :: LexParser (UserType,UserType)
annotResult
  = do keyword ":"
       (tpars,mbteff,tres) <- tresult
       let teff = case mbteff of Nothing -> makeTpTotal (before (getRange tres))
                                 Just tp -> tp
       if (null tpars)
        then return (teff, tres)
        else return (teff, makeTpFun tpars teff tres (combineRanged (map snd tpars) tres))


typeparams
  = do tbinds <- angles tbinders
       return tbinds
  <|>
    do return []


parameters :: Bool -> LexParser ([ValueBinder (Maybe UserType) (Maybe UserExpr)],Range)
parameters allowDefaults
  = parensCommasRng (parameter allowDefaults)

parameter :: Bool -> LexParser (ValueBinder (Maybe UserType) (Maybe UserExpr))
parameter allowDefaults
  = do (name,rng) <- paramid
       tp         <- optionMaybe typeAnnotPar 
       (opt,drng) <- if allowDefaults then defaultExpr else return (Nothing,rangeNull)
       return (ValueBinder name tp opt rng (combineRanges [rng,getRange tp,drng]))

paramid = identifier <|> wildcard

defaultExpr
  = do krng <- keyword "="
       e <- expr
       return (Just e, combineRanged krng e)
  <|>
    return (Nothing,rangeNull)


{--------------------------------------------------------------------------
  Statements
--------------------------------------------------------------------------}

block :: LexParser UserExpr
block 
  = do rng1 <- lcurly    
       many semiColon
       stmts1 <- semis statement
       stmts2 <- do rng2 <- keyword "return"
                    e <- expr
                    semiColons
                    return [StatExpr (makeReturn rng2 e)]
                 <|>
                    return []
       rng2 <- rcurly
       let stats = stmts1 ++ stmts2
       case (last stats) of
         StatExpr exp -> return (Parens (foldr combine exp (init stats)) (combineRange rng1 rng2))
         _            -> fail "Last statement in a block must be an expression"
  where
    combine :: Statement -> UserExpr -> UserExpr
    combine (StatFun f) exp   = f exp
    combine (StatExpr e) exp  = let r = getRange e 
                                in Bind (Def (ValueBinder (newName "_") () e r r) r Private DefVal "") exp r

makeReturn r0 e
  = let r = getRange e
    in App (Var nameReturn False r0) [(Nothing,e)] (combineRange r0 r)

data Statement = StatFun (UserExpr -> UserExpr)
               | StatExpr UserExpr

statement :: LexParser Statement
statement
  = do funs <- many1 (functionDecl rangeNull Private)
       return (StatFun (\body -> Let (DefRec funs) body (combineRanged funs body))) 
  <|>
    do fun <- localValueDecl 
       return (StatFun fun) -- (\body -> -- Let (DefNonRec val) body (combineRanged val body)
                            --              Bind val body (combineRanged val body)  ))
  <|>
    do var <- varDecl 
       return (StatFun (\body -> Bind var body (combineRanged var body)))
  <|> 
    do exp <- compound
       case exp of
         Var name _ rng -> do ann <- typeAnnotation
                              keyword "="
                              e <- expr
                              let val = Def (ValueBinder name () (ann e) rng (combineRanged rng e)) (combineRanged rng e) Private DefVal ""
                              return (StatFun (\body -> Bind val body (combineRanged rng body)))
                           <|>
                           return (StatExpr exp)
         _              -> return (StatExpr exp)
        
localValueDecl 
  = do krng <- keyword "val"
       pat  <- pattern
       keyword "="
       e    <- valexpr
       let bindVar binder mbTp rng
            = let annexpr = case mbTp of
                              Just tp -> Ann e tp rng
                              Nothing -> e
                  vbinder = ValueBinder (binderName binder) () annexpr (binderNameRange binder) (binderRange binder)
              in \body -> Bind (Def vbinder rng Private DefVal "") body (combineRanged krng body)
       case unParens(pat) of
         PatVar (binder@ValueBinder{ binderExpr = PatWild _ })
           -> return $ bindVar binder (binderType binder) (binderRange binder)
         PatAnn (PatVar (binder@ValueBinder{ binderExpr = PatWild _})) tp rng
           -> return $ bindVar binder (Just tp) rng
         _ -> return $ \body -> Case e [Branch pat guardTrue body] (combineRanged krng body)

  where
    unParens (PatParens p _) = unParens(p)
    unParens p               = p         

typeAnnotation :: LexParser (UserExpr -> UserExpr)
typeAnnotation 
  = do tp <- typeAnnot
       return (\e -> Ann e tp (combineRanged e tp))
  <|>
    return id

{--------------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------------}
valexpr :: LexParser UserExpr
valexpr
  = compound <|> funexpr <|> block
  <?> "expression"

expr :: LexParser UserExpr
expr
  = compound <|> funexpr <|> funblock
  <?> "expression"

ifbranch :: LexParser UserExpr
ifbranch
  = returnexpr <|> matchexpr <|> funexpr <|> block  

branchexpr :: LexParser UserExpr
branchexpr
  = returnexpr <|> compound <|> funexpr <|> block


compound
  = do rng <- keyword "if"
       tst <- parens expr
       optional (keyword "then")
       texpr   <- ifbranch
       eexprs  <- many elif
       eexpr   <- do keyword "else"
                     ifbranch
                  <|>
                     return (Var nameUnit False (after (combineRanged texpr (map snd eexprs))))
       let fullMatch = foldr match eexpr ((tst,texpr):eexprs) 
                     where
                       match (tst,texpr) eexpr
                        = Case tst [Branch (PatCon nameTrue [] rangeNull (getRange texpr)) guardTrue texpr
                                   ,Branch (PatCon nameFalse [] rangeNull (getRange eexpr)) guardTrue eexpr]
                                   (combineRanged tst eexpr)
       
       return fullMatch
  <|> 
    matchexpr
  where
    elif
      = do keyword "elif"
           tst <- parens expr
           optional (keyword "then")
           texpr <- ifbranch
           return (tst,texpr)

returnexpr 
  = do rng <- keyword "return"
       exp <- expr
       return (makeReturn rng exp)


matchexpr
  = do rng <- keyword "match"
       tst <- parens expr  -- todo: multiple patterns
       (branches,rng2) <- semiBracesRanged1 branch 
       return (Case tst branches (combineRange rng rng2))
  <|>
    opexpr

{--------------------------------------------------------------------------
  Branches
--------------------------------------------------------------------------}
branch
  = do pat  <- pattern
       grd  <- guard
       keyword "->"
       exp  <- branchexpr
       return (Branch pat grd exp)
  <?> "pattern match"

guard
  = do bar
       expr <?> "guard expression"
  <|>
    do return guardTrue


{--------------------------------------------------------------------------
  Op expr
--------------------------------------------------------------------------}
opexpr :: LexParser UserExpr
opexpr
  = do e1 <- fappexpr
       (do ess <- many1(do{ op <- operatorVar; e2 <- fappexpr; return [op,e2]; }) 
           return (App (Var nameOpExpr True rangeNull) [(Nothing,e) | e <- e1 : concat ess] (combineRanged e1 (concat ess)))
        <|>
           return e1)

fappexpr :: LexParser UserExpr
fappexpr
  = do e <- appexpr
       fargs <- many bfunexpr
       return (injectApply e fargs)       
  where
    injectApply expr []
      = expr
    injectApply expr fargs
      = case expr of
          App fun args rng -> App fun (args ++ nfargs) rng
          _                -> App expr nfargs (combineRanged expr fargs)
      where
        nfargs = [(Nothing,f) | f <- fargs]

appexpr :: LexParser UserExpr
appexpr 
  = do e0 <- prefix
       fs <- many (dotexpr <|> applier <|> indexer)
       return (foldl (\e f -> f e) e0 fs)
  where      
    dotexpr, indexer, applier :: LexParser (UserExpr -> UserExpr)
    dotexpr
      = do keyword "."
           e <- prefix
           (do rng0 <- lparen
               args <- sepBy argument (comma)
               rng1 <- rparen
               return (\arg0 -> App e ((Nothing,arg0):args) (combineRanged arg0 rng1))
            <|>
               return (\arg0 -> App e [(Nothing,arg0)] (combineRanged arg0 e)))

    indexer 
      = do rng0 <- special "["
           idxs <- sepBy1 expr comma
           rng1 <- special "]"
           return (\exp -> App (Var nameIndex False (combineRange rng0 rng1)) (map (\a -> (Nothing,a)) (exp:idxs)) (combineRange rng0 rng1))

    applier 
      = do rng0 <- lparen
           args <- sepBy argument (comma)
           rng1 <- rparen
           return (\exp -> App exp (args) (combineRanged exp rng1))

prefix :: LexParser UserExpr
prefix
  = do ops <- many operatorVar
       aexp <- atom
       return (foldr (\op e -> App op [(Nothing,e)] (combineRanged op e)) aexp ops)


operatorVar
  = do (name,rng) <- qoperator
       return (Var name True rng)
    <|>
    do rng <- keyword ":="
       return (Var nameAssign True rng)   

argument :: LexParser (Maybe (Name,Range),UserExpr)
argument
  = do exp <- aexpr
       case exp of
         Var name _ rng -> do keyword "="
                              exp2 <- expr
                              return (Just (name,rng),exp2)
                           <|>
                              return (Nothing,exp)
         _              -> return (Nothing,exp)

bfunexpr
  = funblock <|> funexpr

funblock
  = do exp <- block 
       return (Lam [] exp (getRange exp))    

funexpr
  = do rng <- keyword "fun" <|> keyword "function"
       spars <- squantifier
       (tpars,pars,parsRng,mbtres,ann) <- funDef 
       body <- block
       let fun = promote spars tpars mbtres
                  (Lam pars body (combineRanged rng body))
       return (ann fun)
  

{--------------------------------------------------------------------------
  Atomic expression
--------------------------------------------------------------------------}
atom :: LexParser UserExpr
atom
  = do (name,rng) <- qidentifier <|> qconstructor
       return (Var name False rng)
  <|>
    do tupleExpr -- must be second due to '(' operator ')'
  <|> 
    do listExpr
  <|>
    do lit <- literal
       return (Lit lit)
  <?> "(simple) expression"

literal
  = do (i,rng) <- integer
       return (LitInt i rng)
  <|>
    do (f,rng) <- floatLit
       return (LitFloat f rng)
  <|> 
    do (s,rng) <- stringLit
       return (LitString s rng)
  <|> 
    do (c,rng) <- charLit
       return (LitChar c rng)
  <?> "constant"


aexpr
  = do e <- expr
       (do keyword ":"
           tp <- ptypescheme
           return (Ann e tp (combineRanged e tp))
        <|>
           return e)
          
          

tupleExpr :: LexParser UserExpr
tupleExpr
  = do rng1 <- lparen <?> ""
       es <- sepEndBy aexpr comma
       rng2 <- rparen
       case es of
         []  -> return (Var nameUnit False (combineRange rng1 rng2))
         [e] -> return (Parens e (combineRanged rng1 rng2))
         _   -> return (App (Var (nameTuple (length es)) False (combineRange rng1 rng2)) [(Nothing,e) | e <- es] (combineRange rng1 rng2))
  

listExpr :: LexParser UserExpr
listExpr
  = do rng1 <- special "[" <?> ""
       es <- sepEndBy aexpr comma
       rng2 <- special "]"
       if null es 
        then return (makeNil (combineRange rng1 rng2))
        else return (adjustRange (combineRange rng1 rng2) (foldr makeCons (makeNil (after rng2)) (es)))

makeNil rng   = Var nameNull False rng
makeCons x xs = makeApp (Var nameCons False (after (getRange x))) [x,xs]

-----------------------------------------------------------
-- Patterns (and binders)
-----------------------------------------------------------
binder :: LexParser (UserExpr -> ValueBinder () UserExpr)
binder
  = do (name,range) <- identifier
       ann <- typeAnnotation 
       return (\expr -> ValueBinder name () (ann expr) range range)

funid
  = identifier
  <|>
    do rng1 <- special "["
       rng2 <- special "]"
       return (nameIndex,combineRange rng1 rng2)


pattern :: LexParser UserPattern
pattern
  = patAnn

patAnn
  = do p <- patAs       
       maybeTypeAnnot p (\tp -> PatAnn p tp (combineRanged p tp))

patAs
  = do p <- patAtom
       (do keyword "as"
           (id,rng) <- identifier
           return (PatVar (ValueBinder id Nothing p rng rng))
        <|>
           return p)

patAtom :: LexParser UserPattern 
patAtom
  = do (name,rng) <- qconstructor
       (ps,r) <- parensRng (sepBy namedPattern (comma)) <|> return ([],rangeNull)
       return (PatCon name ps rng (combineRanged rng r))
  <|>
    do (name,rng) <- identifier     
       tp <- optionMaybe typeAnnot
       return (PatVar (ValueBinder name tp (PatWild rng) rng (combineRanged rng tp)))  -- could still be singleton constructor
  <|>
    do (_,range) <- wildcard
       return (PatWild range)
  <|>
    do (ps,rng) <- parensRng (sepBy namedPattern comma)
       case ps of
         [p] -> return (PatParens (snd p) rng)
         _   -> return (PatCon (nameTuple (length ps)) ps rng rng)
  {-
  <|>
    do lit <- literal
       return (PatLit lit)
  -}

namedPattern :: LexParser (Maybe (Name,Range),UserPattern)
namedPattern
  = do (name,rng) <- try (do{ x <- identifier; keyword "="; return x})
       pat <- pattern
       return (Just (name,rng),pat)
  <|>
    do pat <- pattern
       return (Nothing,pat)

maybeTypeAnnot :: a -> (UserType -> a) -> LexParser a
maybeTypeAnnot def f
  = do tp <- typeAnnot
       return (f tp)
  <|>
    return def




{--------------------------------------------------------------------------
  Types
--------------------------------------------------------------------------}
typeAnnot :: LexParser UserType
typeAnnot
  = do keyword ":"
       ptype

typeAnnotPar :: LexParser UserType
typeAnnotPar
  = do keyword ":"
       (do rng <- specialOp "?"
           tp <- ptype
           return (TpApp (TpCon nameTpOptional rng) [tp] (combineRanged rng tp))
        <|>
        do rng <- specialOp "$"
           ([],eff,res) <- tresultTotal -- todo: use proper result
           return (TpApp (TpCon nameTpDelay rng) [eff,res] (combineRanged rng res))
        <|>
        ptype)

ptypescheme :: LexParser UserType
ptypescheme
 = do tp <- pquanSome <|> ptype
      return (promoteType tp)   -- add quantifiers for free type variables
   <?> "type signature"

ptype :: LexParser UserType
ptype
  = pquanForall
  <|>
    do tqual
  <?> "type"


aquantifier
  = do keyword "forall"
       angles tbinders
  <|> return []

squantifier
  = do keyword "some"
       parens tbinders
  <|> return []


pquanSome
  = pquantifier QSome ptype

pquanForall
  = pquantifier QForall tqual

pquanExists
  = pquantifier QExists ptype

pquantifier quan next
  = do rng <- keyword (case quan of QSome -> "some"; QForall -> "forall"; QExists -> "exists")
       params <- angles tbinders
       -- keyword "."
       tp <- next
       let makeQuan = \tname tp -> TpQuan quan tname tp (combineRanged rng tp)
       return (foldr makeQuan tp params)


tqual
  = do tp  <- tarrow
       pqualifier tp 

pqualifier tp
  = do keyword "with"
       ps <- many1 predicate
       return (TpQual ps tp)
  <|>
    return tp

predicate
  = do tp <- tid
       typeApp tp
  <?> "predicate" -- fail "predicates are not allowed for now"

tarrow :: LexParser UserType
tarrow
  = do (tps,rng1) <- tatom
       (do keyword "->"
           (targs,teff,tres) <- tresultTotal
           return (makeTpFun (tps ++ targs) teff tres (combineRanged rng1 tres))
        <|>
        return (tuple (tps,rng1)))


teffect
  = do rng1   <- langle
       labels <- sepBy tlabel comma
       (ext,brng)    <- textend 
       rng2   <- rangle
       let rng = combineRange rng1 rng2
       return (foldr (makeEffectExtend brng) (ext rng) labels)

textend :: LexParser (Range -> UserType, Range {- "|" -})
textend
  = do brng <- bar
       tp <- teffect <|> tid
       return (const tp,brng)
  <|>
    return (makeEffectEmpty, rangeNull)


tlabel
  = do tp1 <- tid
       tp2 <- typeApp tp1
       return tp2


tresultTotal :: LexParser ([(Name,UserType)],UserType,UserType)
tresultTotal
  = do (targs,mbeff,tres) <- tresult
       let teff = case mbeff of Just tp -> tp
                                Nothing -> makeTpTotal (before (getRange tres))
       return (targs,teff,tres)

tresult :: LexParser ([(Name,UserType)],Maybe UserType,UserType)
tresult
  = do (tps1,rng1)  <- tatom

       (targs,tps2) <-  do ts <- many (do{ keyword "->"; tatom})
                           if null ts
                            then return ([],tps1)
                            else return (merge ((tps1,rng1):init ts), fst (last ts))
       (teff,tres) <- do (tps,rng) <- tatom
                         return (Just (tuple (tps2,rng)), tuple (tps,rng))
                      <|>
                         return (Nothing {-makeTpTotal (getRange (map snd tps2))-}, tuple(tps2,rng1)) -- TODO: range
       return (targs,teff,tres)
  where
    merge :: [([(Name,UserType)],Range)] -> [(Name,UserType)]
    merge ts  = concat (map fst ts)



tuple :: ([(Name,UserType)],Range) -> UserType
tuple ([tp],rng) = snd tp
tuple (tps,rng) = TpApp (TpCon (nameTuple (length tps)) rng) (map snd tps) rng


tatom :: LexParser ([(Name,UserType)],Range)
tatom
  = {- do tp <- listType
       return (single tp)
  <|>
    -}
    do tp1 <- tid
       tp2 <- typeApp tp1
       return (single tp2)
  <|>
    do rng1 <- special "("
       (do tps  <- sepBy paramTypeX comma
           rng2 <- rparen
           {- case tps of
            []  -> return (single (TpCon nameUnit (combineRange rng1 rng2))) 
            _   -> -}
           return ([(name,tp) | (name,rng,tp) <- tps], combineRange rng1 rng2)
        <|>
        do cs <- many1 comma
           rng2 <- rparen
           tp <- typeApp (mktuple (length cs + 1) (combineRange rng1 rng2))
           return (single tp)
        )
  <|>
    do tp <- teffect
       return (single tp)
  where    
    single tp
      = ([(nameNil,tp)],getRange tp)

    mktuple n rng
      = TpCon (unqualify (nameTuple n)) rng   -- unqualify: means regular lookup

typeApp tp
  = do rng1  <- langle -- liparen
       targs <- sepBy anntypek comma 
       rng2  <- rangle
       return (TpApp tp (targs) (combineRanged tp rng2))
  <|>
    do return tp

paramType :: LexParser (Name,Range,UserType)
paramType
  = do (id,rng) <- varid <|> wildcard <|> return (nameNil, rangeNull)
       keyword ":"
       tp <- parameterType rng
       return (id,rng,tp) 

paramTypeX
  = do (id,rng) <- try (do v <- varid <|> wildcard; keyword ":"; return v)
       tp <- parameterType rng
       return (id,rng,tp)
  <|>
    do tp <- parameterType rangeNull
       return (nameNil,getRange tp,tp)


parameterType rng
  = do rng2 <- specialOp "?"
       tp <- ptype
       return (TpApp (TpCon nameTpOptional rng) [tp] (combineRanged rng2 tp))
    <|>
    do ptype

{-      <|>
        do rng2 <- specialOp "$"
           ([],eff,res) <- tresultTotal -- todo: use proper result
           return (id, rng, TpApp (TpCon nameTpDelay rng) [eff,res] (combineRanged rng2 res))
-}           


anntypek
  = do tp <- ptype
       (do specialOp "::"
           kind <- pkind
           return (TpAnn tp kind)
        <|>
           return tp)

tid
  = do (id,rng) <- qvarid
       return (if isTypeVar id then TpVar id rng else TpCon id rng)
  <|>
    do (id,rng) <- wildcard <?> ""
       return (TpVar id rng)

listType
  = do rng1 <- special "["
       ( do tp   <- ptype
            rng2 <- special "]"
            return (TpApp (TpCon nameTpList rng1) [tp] (combineRange rng1 rng2))
        <|>
         do rng2 <- special "]"
            return (TpCon nameTpList (combineRange rng1 rng2))
        )


-- Just before or after a token.
before range
  = makeRange (rangeStart range) (rangeStart range)

after range
  = makeRange (rangeEnd range) (rangeEnd range)


makeTpFun args effect res
  = -- TpApp (TpApp (makeTpApp (TpCon (nameTpFun (length args)) (combineRanged args res)) args) effect) res
    TpFun args effect res
  

makeTpApp tp args
  = TpApp tp args

makeApp expr args
  = App expr [(Nothing,a) | a <- args] (combineRanged expr args)

makeTpPure rng
  = TpCon nameTpPure rng

makeTpTotal rng
  = TpCon nameEffectEmpty rng

makeEffectEmpty rng 
  = TpCon nameEffectEmpty rng

makeEffectExtend rng (label) ext
  = TpApp (TpCon nameEffectExtend rng) [label,ext] (combineRanged (getRange label) ext)

{--------------------------------------------------------------------------
  Type binder
--------------------------------------------------------------------------}
tbinderDef :: LexParser (UserKind -> TypeBinder UserKind)
tbinderDef
  = do (id,rng) <- tbinderId
       return (\kind -> TypeBinder id kind rng rng) 

tbinderId
  = typeid <|> tlist <|> ttuple 
--  <|> toptional <|> tdelay
  <|> temptyOrExtend

tlist
  = do rng1 <- special "["
       rng2 <- special "]"
       return (unqualify nameTpList,combineRange rng1 rng2)  -- unqualify: local lookup? 

ttuple
  = do rng1 <- lparen
       cs   <- many (comma)
       rng2 <- rparen
       return (unqualify (nameTuple (length cs+1)), combineRange rng1 rng2) -- unqualify: local lookup?


temptyOrExtend
  = do rng1 <- langle
       (do bar <?> "extend bar"
           rng2 <- rangle
           return (nameEffectExtend, combineRange rng1 rng2)
        <|>
        do rng2 <- rangle
           return (nameEffectEmpty, combineRange rng1 rng2))


tbinders :: LexParser [TypeBinder UserKind]
tbinders
  = sepBy tbinder (comma)

tbinder :: LexParser (TypeBinder UserKind)
tbinder
  = do (id,rng) <- varid <?> "type parameter"
       kind     <- kindAnnot 
       return (TypeBinder id kind rng rng)

kindAnnot :: LexParser UserKind
kindAnnot 
  = do specialOp "::"
       kind <- pkind
       return kind
  <|>
    return KindNone



{--------------------------------------------------------------------------
  Kinds
--------------------------------------------------------------------------}
pkind :: LexParser UserKind
pkind
  = do params <- parensCommas pkind 
       keyword "->"
       res    <- pkind
       return (foldr KindArrow res params)
  <|>
    do k <- katom
       (do keyword "->"
           res <- pkind
           return (KindArrow k res)
        <|>
        return k)
  <?> "kind"

katom
  = do parensx KindParens pkind
  <|>
    do rng <- specialConId "V" 
       return (KindCon nameKindStar rng)
  <|>
    do rng <- specialConId "X" 
       return (KindCon nameKindLabel rng)
  <|>
    do rng <- specialConId "E"
       return (KindCon nameKindEffect rng)
  <|>
    do rng <- specialConId "H"
       return (KindCon nameKindHeap rng)
  <|>
    do rng <- specialConId "P"
       return (KindCon nameKindPred rng)
  <?> "kind constant (V,E,H,X, or P)"

-----------------------------------------------------------
-- Braces and parenthesis
-----------------------------------------------------------
semiBraces :: LexParser a -> LexParser [a]
semiBraces p
  = do (xs,rng) <- semiBracesRanged p
       return xs

semiBracesRanged :: LexParser a -> LexParser ([a],Range)
semiBracesRanged p
  = do rng1 <- lcurly
       many semiColon
       xs <- sepEndBy p semiColons
       rng2 <- rcurly
       return (xs,combineRange rng1 rng2)

semiBracesRanged1 :: LexParser a -> LexParser ([a],Range)
semiBracesRanged1 p
  = do rng1 <- lcurly
       many semiColon
       xs <- sepEndBy1 p semiColons
       rng2 <- rcurly
       return (xs,combineRange rng1 rng2)

semis p
  = sepEndBy p semiColons

semiColons  
  = many1 semiColon

anglesRanged p
  = bracketed langle rangle (,) p


anglesCommas p
  = angles (sepBy p comma)

angles p
  = bracketed langle rangle const p

parensCommas p
  = parens (sepBy p comma)

parensRng p
  = parensx (,) p

parens p
  = parensx const p

parensCommasRng p
  = parensx (,) (sepBy p comma)

parensx f p
  = bracketed lparen rparen f p

curliesx f p
  = bracketed lcurly rcurly f p

bracketed open close f p
  = do rng1 <- open 
       x <- p
       rng2 <- close
       return (f x (combineRanged rng1 rng2))


-----------------------------------------------------------
-- Lexical tokens
-----------------------------------------------------------
lparen   = special "(" -- <|> liparen
rparen   = special ")"
langle   = specialOp "<" 
rangle   = specialOp ">"
bar      = specialOp "|"
comma    = special ","
lcurly   = special "{"
rcurly   = special "}"

{-
liparen :: LexParser Range
liparen
  = do (Lexeme rng _) <- parseLex LexLIParen
       return rng
-}

semiColon :: LexParser Range
semiColon
  = do (Lexeme rng _) <- parseLex LexInsSemi <|> parseLex (LexSpecial ";")
       return rng
  <?> "semi colon"


-----------------------------------------------------------
-- Identifiers & Operators
-----------------------------------------------------------

identifier
  = ensureUnqualified "identifier" qidentifier


qidentifier :: LexParser (Name,Range)
qidentifier
  = qvarid <|> qidop

qconstructor :: LexParser (Name,Range)
qconstructor
  = qconid

qoperator :: LexParser (Name,Range)
qoperator 
  = qop 
  <|>
    do rng1 <- special "`"
       (name,rng) <- (qidentifier <|> qconstructor)
       rng2 <- special "`"
       return (name,rng {- combineRange rng1 rng2 -})

-----------------------------------------------------------
-- Unqualified Identifiers
-----------------------------------------------------------
varid :: LexParser (Name,Range)
varid
  = ensureUnqualified "identifier" qvarid

idop :: LexParser (Name,Range)
idop
  = ensureUnqualified "operator" qidop

conid :: LexParser (Name,Range)
conid
  = ensureUnqualified "constructor" qconid

op :: LexParser (Name,Range)
op = ensureUnqualified "operator" qop

typeid ::  LexParser (Name,Range)
typeid
  = do (name,rng) <- qtypeid
       if (isQualified name) 
        then fail "qualified type variable"
        else return (name,rng)

ensureUnqualified :: String -> LexParser (Name,Range) -> LexParser (Name,Range)       
ensureUnqualified entity p
  = do (name,rng) <- p
       if (isQualified name) 
        then fail ("qualified " ++ entity)
        else return (name,rng)

-----------------------------------------------------------
-- Lexical tokens
-----------------------------------------------------------
qtypeid
  = try $
    do pos <- getPosition
       (name,range) <- qvarid
       if (not (isTypeVar name))
        then return (name,range)
        else do setPosition pos
                mzero <?> "type name"

qop :: LexParser (Name,Range)
qop
  = do (Lexeme rng (LexOp id)) <- parseLex (LexOp nameNil)
       return (id,rng)
  <?> "operator"


-- is really qvarid, varid, from the spec
qvarid :: LexParser (Name,Range)
qvarid
  = do (Lexeme rng (LexId id)) <- parseLex (LexId nameNil)
       return (id,rng)
  <?> "identifier"

-- is really qidop and idop from the spec
qidop :: LexParser (Name,Range)
qidop
  = do (Lexeme rng (LexIdOp id)) <- parseLex (LexIdOp nameNull)
       return (id,rng)
  <?> ""

-- is really qconid and conid in the spec
qconid :: LexParser (Name,Range)
qconid
  = do (Lexeme rng (LexCons id)) <- parseLex (LexCons nameNil)
       return (id,rng)
  <?> "constructor"

modulepath :: LexParser (Name,Range)
modulepath
  = do (id,rng) <- qvarid
       return (newName (show id), rng) -- return the entire module path as one identifier
  <?> "module path"

wildcard:: LexParser (Name,Range)
wildcard
  = do (Lexeme rng (LexWildCard id)) <- parseLex (LexWildCard nameNil)
       if (show id == "_")
        then return (newName ("_" ++ show (rangeStart rng)), rng)
        else return (id,rng)
  <?> "wildcard"


integer :: LexParser (Integer,Range)
integer
  = do (Lexeme rng (LexInt i)) <- parseLex (LexInt 0)
       return (i,rng)
  <?> "integer"

floatLit :: LexParser (Double,Range)
floatLit
  = do (Lexeme rng (LexFloat f)) <- parseLex (LexFloat 0.0)
       return (f,rng)
  <?> "float"

charLit :: LexParser (Char,Range)
charLit
  = do (Lexeme rng (LexChar c)) <- parseLex (LexChar ' ')
       return (c,rng)
  <?> "character"

stringLit :: LexParser (String,Range)
stringLit
  = do (Lexeme rng (LexString s)) <- parseLex (LexString "")
       return (s,rng)
  <?> "string"


specialOp :: String -> LexParser Range
specialOp s
  = try (
      do (Lexeme rng (LexOp op)) <- parseLex (LexOp nameNil)
         if (show op == s)
          then return rng
          else fail s
      <?> show s
    )

specialId :: String -> LexParser Range
specialId s
  = try (
      do (Lexeme rng (LexId id)) <- parseLex (LexId nameNil)
         if (show id == s)
          then return rng
          else fail s
      <?> show s
    )

specialConId :: String -> LexParser Range
specialConId s
  = try (
      do (Lexeme rng (LexCons id)) <- parseLex (LexCons nameNil)
         if (show id == s)
          then return rng
          else fail s
      <?> show s
    )



special :: String -> LexParser Range
special s
  = do (Lexeme rng _) <- parseLex (LexSpecial s)
       return rng
  <?> show s



keyword :: String -> LexParser Range
keyword s
  = do (Lexeme rng _) <- parseLex (LexKeyword s "")
       return rng
  <?> s

dockeyword :: String -> LexParser (Range,String)
dockeyword s
  = do (Lexeme rng (LexKeyword _ doc)) <- parseLex (LexKeyword s "")
       return (rng,doc)
  <?> s


{--------------------------------------------------------------------------
  Adjust the range of an expression
--------------------------------------------------------------------------}
adjustRange :: Range -> UserExpr -> UserExpr
adjustRange rng expr
  = Parens expr rng


adjustTpRange :: Range -> UserType -> UserType
adjustTpRange rng tp
  = TpParens tp rng


