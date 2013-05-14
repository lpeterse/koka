-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Pretty-printer for core-F 
-}
-----------------------------------------------------------------------------

module Core.Pretty( prettyCore ) where

import Data.Char( isAlphaNum )
import Common.Name
import Common.ColorScheme
import Common.Syntax
import qualified Common.NameSet as S
import Lib.PPrint
import Core.Core
import Kind.Kind
import Kind.Pretty hiding (prettyKind)
import Kind.Synonym
import Kind.ImportMap
import Type.Type
import Type.Pretty

-- import Lib.Trace

{--------------------------------------------------------------------------
  Show pretty names (rather than numbers) 
--------------------------------------------------------------------------}

prettyNames :: Bool
prettyNames = True 

keyword env s
  = color (colorKeyword (colors env)) (text s)

{--------------------------------------------------------------------------
  Show instance declarations 
--------------------------------------------------------------------------}

instance Show Core      where show = show . prettyCore      defaultEnv
instance Show External  where show = show . prettyExternal  defaultEnv
instance Show TypeDef   where show = show . prettyTypeDef   defaultEnv
instance Show DefGroup  where show = show . prettyDefGroup  defaultEnv
instance Show Def       where show = show . prettyDef       defaultEnv
instance Show Expr      where show = show . prettyExpr      defaultEnv
instance Show Lit       where show = show . prettyLit       defaultEnv
instance Show Branch    where show = show . prettyBranch    defaultEnv
instance Show Pattern   where show = show . prettyPattern   defaultEnv

{--------------------------------------------------------------------------
  Pretty-printers proper 
--------------------------------------------------------------------------}

prettyCore :: Env -> Core -> Doc
prettyCore env0 core@(Core name imports fixDefs typeDefGroups defGroups externals doc) 
  = prettyComment env doc $
    keyword env "module" <+> 
    (if (coreIface env) then text "interface " else empty) <>    
    prettyDefName env name <$>
    (vcat $ concat $
      [ map (prettyImport envX) (imports ++ extraImports)
      , map (prettyFixDef envX) fixDefs
      , map (prettyImportedSyn envX) importedSyns
      , map (prettyTypeDefGroup envX) typeDefGroups
      , map (prettyDefGroup env) defGroups
      , map (prettyExternal env) externals
      ]
    )
  where
    env  = env1{ showKinds = True, expandSynonyms = False }
    envX = env1{ showKinds = True, expandSynonyms = True }

    importedSyns = extractImportedSynonyms core
    extraImports = extractImportsFromSynonyms imports importedSyns
    env1         = env0{ importsMap = extendImportMap extraImports (importsMap env0) }
    
prettyImport env imp
  = prettyComment env (importModDoc imp) $
    (if isPublic (importVis imp) then keyword env "public " else empty) <>
    keyword env "import" 
      <+> pretty (importsAlias (importName imp) (importsMap env)) <+> text "="
      <+> prettyName env (importName imp)  
      <+> text "=" <+> prettyLit env (LitString (importPackage imp)) 
      <> semi


prettyFixDef env (FixDef name fixity) 
  = (case fixity of
       FixInfix fix assoc -> ppAssoc assoc <+> pretty fix
       FixPrefix          -> text "prefix"
       FixPostfix         -> text "postfix")
    <+> prettyDefName env name
  where
    ppAssoc AssocLeft    = text "infixl"
    ppAssoc AssocRight   = text "infixr"
    ppAssoc AssocNone    = text "infix"

prettyImportedSyn :: Env -> SynInfo -> Doc
prettyImportedSyn env synInfo
  = ppSynInfo env False True synInfo Private <> semi

prettyExternal :: Env -> External -> Doc
prettyExternal env (External name tp body vis nameRng doc) 
  = prettyComment env doc $
    prettyVis env vis $
    keyword env "external" <+> prettyDefName env name <+> text ":" <+> prettyType env tp <+> prettyEntries body
  where
    prettyEntries [(Default,content)] = keyword env "= inline" <+> prettyLit env (LitString content) <> semi
    prettyEntries entries             = text "{" <$> tab (vcat (map prettyEntry entries)) <$> text "}"
    prettyEntry (target,content)      = ppTarget env target <> keyword env "inline" <+> prettyLit env (LitString content) <> semi

prettyExternal env (ExternalInclude includes range)
  = empty
prettyExternal env (ExternalImport imports range)
  = empty

  {-
    keyword env "external inline" <+> text "{" <$>
    tab (vcat (map prettyInclude includes)) <$>
    text "}"
  where
    prettyInclude (target,content) 
        = ppTarget env target <> prettyLit env (LitString content)
  -}


ppTarget env target
  = case target of
      Default -> empty
      _       -> keyword env (show target) <> space

prettyTypeDefGroup :: Env -> TypeDefGroup -> Doc
prettyTypeDefGroup env (TypeDefGroup defs) 
  = -- (if (length defs==1) then id else (\ds -> text "rec {" <$> tab ds <$> text "}")) $
    vcat (map (prettyTypeDef env) defs)

prettyTypeDef :: Env -> TypeDef -> Doc
prettyTypeDef env (Synonym synInfo vis )  
  = ppSynInfo env True True synInfo vis <> semi

prettyTypeDef env (Data dataInfo vis conViss )
  = -- keyword env "type" <+> prettyVis env vis <> ppDataInfo env True dataInfo
    prettyDataInfo env True True dataInfo vis conViss <> semi

prettyDefGroup :: Env -> DefGroup -> Doc
prettyDefGroup env (DefRec defs)   
  = -- (\ds -> text "rec {" <$> tab ds <$> text "}") $
    prettyDefs env defs
    
prettyDefGroup env (DefNonRec def) 
  = prettyDef env def 

prettyDefs :: Env -> Defs -> Doc
prettyDefs env (defs) 
  = vcat (map (prettyDef env) defs)

prettyDef :: Env -> Def -> Doc
prettyDef env (Def name scheme expr vis sort nameRng doc) 
  = prettyComment env doc $
    prettyVis env vis $
    keyword env (show sort) 
    <+> (if nameIsNil name then text "_" else prettyDefName env name)
    <+> text ":" <+> prettyType env scheme 
    <> (if coreIface env then empty else linebreak <> indent 2 (text "=" <+> prettyExpr env expr)) <> semi

prettyVis env vis doc
  = case vis of
      Public  -> if (coreIface env) then doc else (keyword env "public" <+> doc)
      Private -> if (coreIface env) then empty else doc 

prettyType env tp
  = head (prettyTypes env [tp])
 
prettyTypes env tps
  = niceTypes env tps

prettyKind env prefix kind
  = if (kind == kindStar)
     then empty
     else text prefix <+> ppKind (colors env) precTop kind

{--------------------------------------------------------------------------
  Expressions 
--------------------------------------------------------------------------}
tab doc
  = indent 2 doc


prettyExpr :: Env -> Expr -> Doc

-- Core lambda calculus
prettyExpr env lam@(Lam tnames expr) 
  = pparens (prec env) precArrow $
    tab (keyword env "fun" <> tupled [prettyTName env' tname | tname <- tnames] <> text "{" <+> prettyExpr env expr) <+> text "}"
  where
    env'  = env { prec = precTop }
    env'' = env { prec = precArrow }

prettyExpr env (Var tname varInfo) 
  = prettyVar env tname <> prettyInfo 
  where
    prettyInfo 
      = case varInfo of
          InfoNone -> empty
          InfoArity m n -> braces (pretty m <> comma <> pretty n)
          InfoExternal f -> empty

prettyExpr env (App a args) 
  = pparens (prec env) precApp $
    prettyExpr (decPrec env') a <> tupled [prettyExpr env' a | a <- args]
  where
    env' = env { prec = precApp }

-- Type abstraction/application
prettyExpr env (TypeLam tvs expr)
  = pparens (prec env) precArrow $
    keyword env "forall" <> tupled [prettyTypeVar env' tv | tv <- tvs] <+> prettyExpr env' expr 
  where
    env' = env { prec = precTop 
               , nice = (if prettyNames then niceTypeExtendVars tvs else id) $ nice env
               }

prettyExpr env (TypeApp expr tps)
  = pparens (prec env) precApp $
    prettyExpr (decPrec env') expr <> angled [prettyType env'' tp | tp <- tps] 
  where
    env' = env { prec = precApp }
    env'' = env { prec = precTop } 

-- Literals and constants
prettyExpr env (Con tname repr)
  = -- prettyTName env tname 
    prettyVar env tname

prettyExpr env (Lit lit)
  = prettyLit env lit

-- Let
prettyExpr env (Let ([DefNonRec (Def x tp e vis isVal nameRng doc)]) e')
  = vcat [ text "let" <+> nest 2 (prettyName env x <+> text ":" <+> prettyType env tp <$> text "=" <+> prettyExpr env e <> semi)
         , prettyExpr env e' 
         ]
prettyExpr env (Let defGroups expr)
  = vcat [ text "let" <+> vcat (map (prettyDefGroup env) defGroups)
         , text "in" <+> prettyExpr env expr
         ]


-- Case expressions
prettyExpr env (Case exprs branches)
  = text "case" <+> hsep (map (prettyExpr env{ prec = precAtom }) exprs) <+> text "of" <+> prettyBranches env branches

prettyVar env tname
  = prettyName env (getName tname) -- <> braces (ppType env{ prec = precTop } (typeOf tname))

{--------------------------------------------------------------------------
  Case branches 
--------------------------------------------------------------------------}

prettyBranches :: Env -> [Branch] -> Doc
prettyBranches env (branches) 
  = vcat (map (prettyBranch env) branches)

prettyBranch :: Env -> Branch -> Doc
prettyBranch env (Branch patterns guards) 
  = hsep (map (prettyPattern env{ prec = precApp } ) patterns) <> vcat (map (prettyGuard env) guards)

prettyGuard   :: Env -> Guard -> Doc
prettyGuard env (Guard test expr)
  = ( if (isExprTrue test) 
       then empty 
       else text " |" <+> prettyExpr env{ prec = precTop } test
    )   <+> text "->" <+> prettyExpr env{ prec = precTop } expr

prettyPattern :: Env -> Pattern -> Doc
prettyPattern env pat
  = case pat of
      PatCon tname args repr targs info
                        -> -- pparens (prec env) precApp $
                           prettyName env (getName tname) <> tupled (map (prettyPattern (decPrec env)) args)
      PatVar tname PatWild  -> prettyName env (getName tname)
      PatVar tname pat      -> pparens (prec env) precApp $
                               prettyPattern (decPrec env) pat <+> keyword env "as" <+> prettyName env (getName tname)
      PatWild               -> text "_"
  where
    commaSep :: [Doc] -> Doc
    commaSep = hcat . punctuate comma
    prettyArg :: TName -> Doc
    prettyArg tname = parens (prettyName env (getName tname) <+> text "::" <+> prettyType env (typeOf tname))

{--------------------------------------------------------------------------
  Literals 
--------------------------------------------------------------------------}

prettyLit :: Env -> Lit -> Doc
prettyLit env lit 
  = case lit of
      LitInt    i -> color (colorNumber (colors env)) (text (show i))
      LitFloat  d -> color (colorNumber (colors env)) (text (show d))
      LitChar   c -> color (colorString (colors env)) (text (show c))
      LitString s -> color (colorString (colors env)) (text (show s))

{--------------------------------------------------------------------------
  Pretty-printers for non-core terms 
--------------------------------------------------------------------------}

prettyTName :: Env -> TName -> Doc
prettyTName env (TName name tp)
  = prettyName env name -- pretty name -- <> text ":" <> ppType env tp

prettyName :: Env -> Name -> Doc
prettyName env name
  = color (colorSource (colors env)) $
    pretty name

prettyDefName :: Env -> Name -> Doc
prettyDefName env name
  = color (colorSource (colors env)) $
    fmtName (unqualify name) 
  where
    fmtName cname
      = let (name,postfix) = canonicalSplit cname
            s = show name
            pre = case s of
                   ""  -> empty
                   (c:cs) -> if (isAlphaNum c || c == '_' || c == '(' || c == '[') then text s else parens (text s)
        in (if null postfix then pre else (pre <+> text postfix))                        
    
ppOperatorName env name
  = color (colorSource (colors env)) $
    fmtName (unqualify name) 
  where
    fmtName name
      = let s = show name
        in case s of
             "" -> empty
             (c:cs) -> if (isAlphaNum c) then text ("`" ++ s ++ "`") else text s

    
prettyTypeVar :: Env -> TypeVar -> Doc
prettyTypeVar
  = ppTypeVar 

 
{--------------------------------------------------------------------------
  Precedence 
--------------------------------------------------------------------------}

type Prec = Int

decPrec :: Env -> Env
decPrec env = env { prec = prec env - 1 }


-- Extend an import map with nice aliases for a set of new imports
extendImportMap :: [Import] -> ImportMap -> ImportMap
extendImportMap imports impMap
  = foldr extend impMap imports
  where
    extend imp impMap 
      = let fullName = importName imp in
        case importsExtend fullName fullName impMap of
         Just newMap -> newMap
         Nothing     -> impMap -- already imported ?

-- extract all qualifiers in synonyms: it can be the case that a type synonym
-- refers to a type defined in a non-imported module and we need to add it to the
-- imports of the .kki file. (no need to add it to the generated code since
-- javascript does not use types, while c-sharp does not use import declarations but fully
-- qualified names all the time)
extractImportsFromSynonyms :: [Import] -> [SynInfo] -> [Import]
extractImportsFromSynonyms imps syns
  = let quals = filter (\nm -> not (S.member nm impNames)) $
                concatMap extractSyn syns
        extraImports = map (\nm -> Import nm "" Private "") quals -- TODO: import path ?        
    in extraImports 
  where
    impNames        = S.fromList (map importName imps)

    extractSyn syn  = qualifier (synInfoName syn) : extractType (synInfoType syn)
    extractType tp
      = case tp of
          TSyn syn args body -> qualifier (typesynName syn) : extractTypes (body:args)
          TApp con args      -> extractTypes (con:args)
          TFun args eff res  -> extractTypes (res:eff:map snd args)
          TForall _ _ body   -> extractType body
          TCon tcon          -> [qualifier (typeConName tcon)]
          TVar _             -> []
    extractTypes tps
      = concatMap extractType tps


-- extract from type signatures the synonyms so we can compress .kki files 
-- by locally defining imported synonyms
extractImportedSynonyms :: Core -> [SynInfo] 
extractImportedSynonyms core
  = let syns = filter (\info -> coreProgName core /= qualifier (synInfoName info)) $
               synonymsToList $ extractSynonyms (extractSignatures core)
    in -- trace ("extracted synonyms: " ++ show (map (show . synInfoName) syns)) $
       syns
  where
    extractSynonym :: Type -> Synonyms
    extractSynonym tp
      = case tp of
          TSyn syn args body  -> let syns = extractSynonyms (body:args) in
                                 case typesynInfo syn of
                                   Just info -> synonymsExtend info syns
                                   Nothing   -> syns
          TApp con args       -> extractSynonyms (con:args)
          TFun args eff res   -> extractSynonyms (res:eff:map snd args)
          TForall _ _ body    -> extractSynonym body
          _                   -> synonymsEmpty

    extractSynonyms :: [Type] -> Synonyms
    extractSynonyms xs = foldr synonymsCompose synonymsEmpty (map extractSynonym xs)
