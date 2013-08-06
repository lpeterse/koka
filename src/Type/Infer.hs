{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Type inference. Relies on results from kind inference 
-----------------------------------------------------------------------------

module Type.Infer (inferTypes, coreVarInfoFromNameInfo ) where

import qualified Lib.Trace
import Data.List(partition,sortBy)
import Lib.PPrint
import Common.Failure
import Common.Error
import Common.Name
import Common.NamePrim( nameTpOptional, nameOptional, nameOptionalNone, nameCopy, nameTpDelay
                      , nameReturn, nameRef, nameByref, nameDeref 
                      , nameRefSet, nameAssign, nameTpUnit, nameTuple
                      , namePatternMatchError, nameSystemCore )
import Common.Range
import Common.Unique
import Common.Syntax
import qualified Common.NameSet as S
import qualified Data.Map as M

import Syntax.Syntax
import qualified Core.Core as Core

import Kind.Kind
import Kind.Constructors
import Kind.Synonym
import Kind.Newtypes
import Kind.ImportMap

import Type.Type
import Type.Pretty
import Type.Assumption
import Type.TypeVar
import qualified Type.Operations as Op
import Type.InferMonad

import Core.AnalysisMatch( analyzeBranches ) 

import Core.Divergent( analyzeDivergence )
import Core.BindingGroups( regroup )
import Core.Simplify( simplify )

import qualified Syntax.RangeMap as RM

trace s x =
  Lib.Trace.trace s 
    x

{--------------------------------------------------------------------------
  Infer Types
--------------------------------------------------------------------------}
inferTypes :: Env -> Maybe RM.RangeMap -> Synonyms -> Newtypes -> Constructors -> ImportMap -> Gamma -> Name -> Int -> DefGroups Type
                -> Error (Gamma, Core.DefGroups, Int, Maybe RM.RangeMap )
inferTypes prettyEnv mbRangeMap syns newTypes cons imports gamma0 context uniq0 defs
  = -- error "Type.Infer.inferTypes: not yet implemented"
    -- return (gamma0,[],uniq0)
    do ((gamma1, coreDefs),uniq1,mbRm) <- runInfer prettyEnv mbRangeMap syns newTypes imports gamma0 context uniq0 
                                          (inferDefGroups True (arrange defs))
       return (gamma1,coreDefs,uniq1,mbRm)
  where
    arrange defs
      = if (context /= nameSystemCore)
         then defs
         else -- pull in front certain key definitions that are used in functions generated from constructors (like accessors)
              let (first,rest) = partition isKeyDef defs
              in first ++ rest

    isKeyDef (DefNonRec def)  = defName def `elem` (map unqualify keyDefNames)
    isKeyDef _                = False

    keyDefNames = [namePatternMatchError,nameRef,nameRefSet,nameDeref]

{--------------------------------------------------------------------------
  Definition groups
--------------------------------------------------------------------------}
inferDefGroups :: Bool -> DefGroups Type -> Inf (Gamma,Core.DefGroups)
inferDefGroups topLevel (defGroup : defGroups)
  = inferDefGroupX topLevel defGroup (inferDefGroups topLevel defGroups)
       -- (g,cores) <- extendGamma core (inferDefGroups topLevel defGroups)
       -- return (g,core:cores)
inferDefGroups topLevel []
  = do gamma <- getGamma
       return (gamma,[])


inferDefGroupX :: Bool -> DefGroup Type -> Inf (Gamma,Core.DefGroups) -> Inf (Gamma,Core.DefGroups)
inferDefGroupX topLevel defGroup cont
  = do (cgroups0,(g,cgroups1)) <- inferDefGroup topLevel defGroup cont
       -- resetUnique
       -- zapSubst
       return (g,cgroups0 ++ cgroups1)

inferDefGroup :: Bool -> DefGroup Type -> Inf a -> Inf ([Core.DefGroup], a)
inferDefGroup topLevel (DefNonRec def) cont
  = -- trace "infer single:" $
    do core <- inferDef Generalized def
       mod  <- getModuleName

       (x,core1) <- let cgroup = [Core.DefNonRec core] 
                    in if topLevel 
                           then let core0 = core{ Core.defName = qualify mod (Core.defName core) }
                                in extendGammaCore False {- already canonical? -} [Core.DefNonRec core0] $
                                    do coreDef <- fixCanonicalName core0
                                       x <- cont
                                       return (x,coreDef)
                           else do x <- extendInfGammaCore topLevel [Core.DefNonRec core] cont
                                   return (x,core)
       addRangeInfoCoreDef topLevel mod def core1
       let cgroup1 = Core.DefNonRec core1
       return ([cgroup1],x)
inferDefGroup topLevel (DefRec defs) cont
  = -- trace "infer group:" $
    do (gamma,infgamma) <- createGammas [] [] defs
       --coreDefs0 <- extendGamma gamma (mapM (inferRecDef topLevel infgamma) defs)
       (coreDefsX,assumed) <- extendGamma False gamma $ extendInfGamma topLevel infgamma $
                                 do assumed <- mapM (\def -> lookupInfName (getName def)) defs
                                    coreDefs0 <- mapM (\def -> inferDef Instantiated def) defs
                                    coreDefs1 <- mapM fixCanonicalName coreDefs0
                                    return (coreDefs1,assumed)
       -- re-analyze the mutual recursive groups
       scoreDefsX <- subst coreDefsX
       let coreGroups0 = regroup scoreDefsX
       -- now analyze divergence
       (coreGroups1,divTNames)
            <- fmap unzip $ 
               mapM (\cgroup -> case cgroup of
                                   Core.DefRec cdefs | analyzeDivergence cdefs -> do cdefs' <- addDivergentEffect cdefs
                                                                                     return (Core.DefRec cdefs',map Core.defTName cdefs')
                                   _ -> return (cgroup,[])) $
               coreGroups0
       -- build a mapping from core name to original definition and assumed type
       -- hack: we map from the name range since there may be overloaded names, and the types are not fully determined yet..
       let coreMap = M.fromList (map (\(def,tp) -> (binderNameRange (defBinder def), (def,tp))) (zip defs assumed))
       -- check assumed types agains inferred types
       coreGroups2 <- mapMDefs (\cdef -> inferRecDef2 topLevel cdef ((Core.defTName cdef) `elem` concat divTNames) (find (Core.defNameRange cdef) coreMap)) coreGroups1
       -- add range info (for documentation)
       mod <- getModuleName
       mapMDefs_ (\cdef -> addRangeInfoCoreDef topLevel mod (fst (find (Core.defNameRange cdef) coreMap)) cdef) coreGroups2
       -- fix local info in the core
       let coreGroups3 = coreGroups2
       -- extend gamma
       x <- (if topLevel then extendGammaCore True {- already canonical -} else extendInfGammaCore False {-toplevel -}) coreGroups3 cont
       return (coreGroups3,x)

       {-
       coreDefsY <- if analyzeDivergence coreDefsX
                     then addDivergentEffect coreDefsX
                     else return coreDefsX
       coreDefs1 <- mapM inferRecDef2 (zip (zip defs coreDefsY) assumed) 
       mod <- getModuleName
       mapM_ (\(def,coreDef) -> addRangeInfoCoreDef topLevel mod def coreDef) (zip defs coreDefs1 )
       let cgroup  = fixLocalInfo topLevel mod $ Core.DefRec coreDefs1
       x <- (if topLevel then extendGammaCore else extendInfGammaCore) cgroup cont
       return (cgroup,x)
       -}
  where
    -- we use a bit of trickery here:
    -- * things on toplevel with full types get added to the gamma since only the gamma can distinguish
    --   multiple recursive definitions with the same overloaded name
    --   this can only be done on toplevel, or otherwise we may do the scoping wrong with
    --   respect to infgamma
    -- * anything else gets added to infgamma -- this means that in a toplevel recursive
    --   group, some defs end up in infgamma and others in gamma: but at the toplevel that 
    --   is ok while infering the types of the recursive group. Eventually, all inferred
    --   types will end up in gamma.
    createGammas :: [(Name,NameInfo)] -> [(Name,NameInfo)] -> [Def Type] -> Inf ([(Name,NameInfo)],[(Name,NameInfo)])
    createGammas gamma infgamma []
      = return (reverse gamma, reverse infgamma)
    createGammas gamma infgamma (Def (ValueBinder name () expr nameRng vrng) rng vis sort doc : defs)
      = case (lookup name infgamma) of
          (Just _) 
            -> do env <- getPrettyEnv
                  if topLevel
                   then infError nameRng (text "recursive functions with the same overloaded name must all have a full type signature" <+> parens (ppName env name) <$>
                                          text " hint: give a type annotation for each function (including the effect type).") 
                   else infError nameRng (text "recursive functions with the same overloaded name cannot be defined as local definitions" <+> parens (ppName env name) <$>
                                          text " hint: use different names for each function.") 

          Nothing 
            -> case expr of
                  Ann _ tp _  | topLevel && tvsIsEmpty (ftv tp)
                    -> do qname <- qualifyName name
                          let nameInfo = createNameInfoX qname sort nameRng tp -- (not topLevel || isValue) nameRng tp  -- NOTE: Val is fixed later in "FixLocalInfo"
                          -- trace ("*** createGammas: assume: " ++ show name ++ ": " ++ show nameInfo) $ return ()
                          createGammas ((name,nameInfo):gamma) infgamma defs
                  _ -> case lookup name gamma of
                         Just _ 
                          -> do env <- getPrettyEnv
                                infError nameRng (text "recursive functions with the same overloaded name must have a full type signature" <+> parens (ppName env name)) 
                         Nothing
                          -> do qname <- if (topLevel) then qualifyName name else return name
                                info <- case expr of
                                          Ann _ tp _ -> return (createNameInfoX qname sort nameRng tp)  -- may be off due to incomplete type: get fixed later in inferRecDef2
                                          _          -> do tp <- Op.freshTVar kindStar Meta                          
                                                           return (createNameInfoX qname DefVal nameRng tp)  -- must assume Val for now: get fixed later in inferRecDef2
                                -- trace ("*** createGammas: assume: " ++ show name ++ ": " ++ show tp) $ return ()
                                createGammas gamma ((qname,info):infgamma) defs

fixCanonicalName :: Core.Def -> Inf Core.Def
fixCanonicalName def  
  = do (_,_,info) <- resolveName (Core.defName def) (Just (Core.defType def, Core.defNameRange def)) (Core.defNameRange def) -- should never fail
       let cname = infoCanonicalName (Core.defName def) info
       return (def{ Core.defName = cname })

mapMDefs :: Monad m => (Core.Def -> m Core.Def) -> Core.DefGroups -> m Core.DefGroups
mapMDefs f cgroups
  = mapM (\cgroup -> case cgroup of
                       Core.DefRec cdefs   -> do cdefs' <- mapM f cdefs
                                                 return (Core.DefRec cdefs')
                       Core.DefNonRec cdef -> do cdef' <- f cdef
                                                 return (Core.DefNonRec cdef')) cgroups

mapMDefs_ :: Monad m => (Core.Def -> m ()) -> Core.DefGroups -> m ()
mapMDefs_ f cgroups
  = mapM_ (\cgroup -> case cgroup of
                        Core.DefRec cdefs   -> mapM_ f cdefs
                        Core.DefNonRec cdef -> f cdef) cgroups


addRangeInfoCoreDef topLevel mod def coreDef
  = let qname = if (topLevel && not (isQualified (Core.defName coreDef)))
                 then qualify mod (Core.defName coreDef)
                 else Core.defName coreDef
    in do addRangeInfo (Core.defNameRange coreDef) (RM.Id qname (RM.NIValue (Core.defType coreDef)) True)
          addRangeInfo (defRange def) (RM.Decl (if defIsVal def then "val" else "fun") qname (RM.mangle qname (Core.defType coreDef)))
       

-- | Add divergent effect to the type of the core definitions
-- Should really fully instantiate and eta-expand to insert evidence
-- but for now, we just fix up the types as necessary without evidence insertion
addDivergentEffect :: [Core.Def] -> Inf [Core.Def]
addDivergentEffect coreDefs0
  = mapM addDiv coreDefs0
  where
    addDiv def
      = do let rng = Core.defNameRange def
           (tp0,_,coref) <- instantiateNoEx rng (Core.defType def) -- no effect extension or otherwise div can be added even if the user specified total for example.
           case splitFunType tp0 of
             Nothing 
              -> -- failure ( "Type.Infer.addDivergentEffect: unexpected non-function type:\n " ++ show coreDefs0) -- ?? should never happen?
                 -- can happen if a value contains a data structure containing recursive functions that refer to the value
                 return def 
             Just (targs,teff,tres)
              -> do -- trace ("addDivergent: " ++ show (Core.defName def) ++ ": " ++ show (Core.defType def, tp0)) $ return ()
                    -- seff <- subst teff
                    -- let snewEff = effectExtendNoDup typeDivergent seff
                    tv <- freshEffect
                    let newEff = effectExtend typeDivergent tv
                    inferUnify (checkEffectSubsume rng) rng newEff teff
                    snewEff <- subst newEff
                    let tp1 = TFun targs snewEff tres
                    (resTp,resCore) <- generalize rng rng typeTotal (TFun targs snewEff tres) (coref (Core.defExpr def))
                    -- inferSubsume (checkEffectSubsume rng) rng (Core.defType def) resTp
                    -- fix up the core since the recursive tname still refers to the old type without the 'div' effect
                    -- let name = unqualify (Core.defName def)
                    --    resCore1 = (Core.|~>) [(name, Core.Var (Core.TName name resTp) Core.InfoNone)] resCore
                    return (def{ Core.defType = resTp, Core.defExpr = resCore })



{--------------------------------------------------------------------------
  Definition
--------------------------------------------------------------------------}
inferRecDef2 :: Bool -> Core.Def -> Bool -> (Def Type,Maybe (Name,Type)) -> Inf (Core.Def)
inferRecDef2 topLevel coreDef divergent (def,mbAssumed)
   = -- trace (" infer rec def: " ++ (if divergent then "div " else "") ++ show (defName def) ++ ": " ++ show (Core.defType coreDef)) $
     do let rng = defRange def
            nameRng = binderNameRange (defBinder def)
        (resTp0,assumedTp,coref0) 
                        <- case mbAssumed of
                            Nothing 
                              -> return (Core.defType coreDef, Core.defType coreDef, id)
                            Just (_,assumed) 
                              -> do assumedTp     <- subst assumed
                                    (resTp,coref) <- inferSubsume (checkRec rng) nameRng assumedTp (Core.defType coreDef) 

                                    -- trace (" infer subsume: " ++ show (Core.defName coreDef) ++ ": " ++ show (assumedTp, Core.defType coreDef)) $ return ()
                                    return (resTp,assumedTp,coref)
              
        (resTp1,resCore1) <- generalize rng nameRng typeTotal resTp0 (coref0 (Core.defExpr coreDef)) -- typeTotal is ok since only functions are recursive (?)

        let name = Core.defName coreDef
            csort = if (topLevel || Core.isTopLevel coreDef) then Core.defSort coreDef else DefVal   
            info = coreVarInfoFromNameInfo (createNameInfoX name csort (defRange def) resTp1)
        (resTp2,coreExpr) 
              <- case (mbAssumed,resCore1) of
                         (Just (_,(TVar _)), Core.TypeLam tvars expr)  -- we assumed a monomorphic type, but generalized eventually
                            -> -- fix it up by adding the polymorphic type application
                               do assumedTpX <- subst assumedTp >>= normalize -- resTp0
                                  -- resTpX <- subst resTp0 >>= normalize 
                                  coreX <- subst $ simplify expr
                                  let -- coreX = simplify expr -- coref0 (Core.defExpr coreDef)
                                      mvars = [TypeVar id kind Meta | TypeVar id kind _ <- tvars]              
                                      msub  = subNew (zip tvars (map TVar mvars)) 


                                      resCoreX = (Core.|~>) [(Core.TName ({- unqualify -} name) assumedTpX, 
                                                              Core.TypeApp (Core.Var (Core.TName ({- unqualify -} name) (resTp1)) info) (map TVar tvars))] -- TODO: wrong for unannotated polymorphic recursion: see codegen/wrong/rec2
                                                 (msub |-> coreX)

                                      bsub  = subNew (zip mvars (map TVar tvars))
                                      resCoreY = Core.TypeLam tvars (bsub |-> resCoreX)
                                  -- trace (" substitute typeapp\n" ++ show (resTpX, assumedTpX, msub |-> coreX)) $ return ()
                                  -- generalize rng nameRng typeTotal resTp0 resCoreX
                                  return (resTp1,resCoreY)
                               {-       
                                  let resCore2 = Core.TypeLam tvars ((Core.|~>) [(Core.TName (unqualify name) resTp1, Core.TypeApp (Core.Var (Core.TName (unqualify name) (resTp1)) Core.InfoNone) (map TVar tvars))] expr) 
                                  trace ("\n ~> \n" ++ show resCore2) $
                                   return resCore2
                               -}
                         (Just (_,_), _) | divergent  -- we added a divergent effect, fix up the occurrences of the assumed type
                            -> do assumedTpX <- normalize assumedTp >>= subst -- resTp0
                                  coreX <- subst $ simplify resCore1
                                  let resCoreX = (Core.|~>) [(Core.TName ({- unqualify -} name) assumedTpX, Core.Var (Core.TName ({- unqualify -} name) resTp1) info)] coreX
                                  return (resTp1, resCoreX)
                         (Just _,_)  -- ensure we insert the right info  (test: static/div2-ack)
                            -> do assumedTpX <- normalize assumedTp >>= subst 
                                  coreX <- subst $ simplify resCore1
                                  let resCoreX = (Core.|~>) [(Core.TName ({- unqualify -} name) assumedTpX, Core.Var (Core.TName ({- unqualify -} name) resTp1) info)] coreX
                                  return (resTp1, resCoreX)
                         (Nothing,_)
                            ->    return (resTp1,resCore1) -- (Core.|~>) [(unqualify name, Core.Var (Core.TName (unqualify name) resTp1) Core.InfoNone)] resCore1 

 
        -- coref2      <- checkEmptyPredicates rng
        -- resTp2      <- subst resTp1
        coreDef2    <- subst (Core.Def (Core.defName coreDef) resTp2 coreExpr (Core.defVis coreDef) csort (Core.defNameRange coreDef) (Core.defDoc coreDef))
        return (coreDef2)

inferRecDef :: Bool -> [(Name,NameInfo)] -> Def Type -> Inf Core.Def
inferRecDef topLevel infgamma def 
  = -- trace ("inferRecDef: " ++ show (getName def)) $
    do let rng = defRange def
           nameRng = binderNameRange (defBinder def)
       eitherRes <- 
          extendInfGamma topLevel infgamma $                   
          do mbAssumedType <- lookupInfName (getName def)
             coreDef <- inferDef Instantiated def
             case mbAssumedType of
               Nothing -- there was a full type signature that has already been taken care of
                -> -- trace "no assumed type" $ 
                    return (Left coreDef)
               Just (qname,assumed) -- otherwise, we need assure it matches the returned type
                -> case assumed of
                    TVar tv 
                      -> {- if (not topLevel)
                          then do inferUnify (checkRec rng) nameRng assumed (Core.defType coreDef) 
                                  return (Left coreDef)
                                  -- return (Left (coreDef{ Core.defType = resTp0, Core.defExpr = coref0 (Core.defExpr coreDef) }))
                          else 
                        -}
                          do -- trace (" match recursive assumed type") $ return ()
                             (resTp0,coref0) <- inferSubsume (checkRec rng) nameRng assumed (Core.defType coreDef) 
                             return (Right (resTp0,coreDef,coref0 (Core.defExpr coreDef)))                                  
                    _  -> return (Left coreDef) -- the user gave a type signature but it ended up in infgamma anyways
       case eitherRes of
          Left cdef 
            -> return cdef
          Right (resTp0,coreDef,resCore0)
            -> -- trace ("right recursive: " ++ show (Core.defName coreDef)) $
               do (resTp1,resCore1) <- generalize rng nameRng typeTotal resTp0 resCore0 -- typeTotal is ok since only functions are recursive (?)
                      
                  let name     = Core.defName coreDef
                      coreExpr = case resCore1 of
                                   Core.TypeLam tvars expr
                                      -> -- trace ("substitute typeapp" ++ show resCore1) $
                                          Core.TypeLam tvars ((Core.|~>) [(Core.TName (unqualify name) (Core.defType coreDef), Core.TypeApp (Core.Var (Core.TName (unqualify name) (resTp1)) Core.InfoNone) (map TVar tvars))] expr) 
                                   _  -> resCore1

                  coref2      <- checkEmptyPredicates rng
                  resTp2      <- subst resTp1
                  coreDef2    <- subst (Core.Def (Core.defName coreDef) resTp2 (coref2 coreExpr) (Core.defVis coreDef) (Core.defSort coreDef) (Core.defNameRange coreDef) (Core.defDoc coreDef))
                  
                  if (False && not topLevel && not (Core.isTopLevel coreDef2) && not (isRho (Core.typeOf coreDef2)))
                   then do -- trace ("local rec with free vars: " ++ show coreDef2) $ return ()
                           typeError rng nameRng (text "local recursive definitions with free (type) variables cannot have a polymorphic type" <$> 
                                                  text " hint: make the function a top-level definition?" ) (Core.typeOf coreDef2) []
                   else return ()

                  return (coreDef2)


inferDef :: Expect -> Def Type -> Inf Core.Def
inferDef expect (Def (ValueBinder name mbTp expr nameRng vrng) rng vis sort doc)
 =do
     penv <- getPrettyEnv
     if (verbose penv >= 2) 
      then Lib.Trace.trace ("infer: " ++ show sort ++ " " ++ show name) $ return ()
      else return ()
     withDefName name $
      (if (sort /= DefFun || nameIsNil name) then id else allowReturn True) $
        do (tp,eff,coreExpr) <- inferExpr Nothing expect expr
                                --  Just annTp -> inferExpr (Just (annTp,rng)) (if (isRho annTp) then Instantiated else Generalized) (Ann expr annTp rng)
            
           (resTp,resCore) <- maybeGeneralize rng nameRng eff expect tp coreExpr -- may not have been generalized due to annotation
           inferUnify (checkValue rng) nameRng typeTotal eff           
           -- trace (" inferred: " ++ show (pretty tp, pretty resTp)) $
           subst (Core.Def name resTp resCore vis   sort nameRng doc)  -- must 'subst' since the total unification can cause substitution. (see test/type/hr1a)

inferBindDef :: Def Type -> Inf (Effect,Core.Def)
inferBindDef (Def (ValueBinder name () expr nameRng vrng) rng vis sort doc)
  = -- trace ("infer bind def: " ++ show name) $
    do withDefName name $
        do (tp,eff,coreExpr) <- inferExpr Nothing Instantiated expr
                                --  Just annTp -> inferExpr (Just (annTp,rng)) Instantiated (Ann expr annTp rng)           
           coreDef <- if (sort /= DefVar) 
                       then return (Core.Def name tp coreExpr vis sort nameRng doc)
                       else do hp <- Op.freshTVar kindHeap Meta
                               (qrefName,_,info) <- resolveName nameRef Nothing rng
                               let refTp = typeApp typeRef [hp,tp]
                                   refVar = coreExprFromNameInfo qrefName info
                                   refExpr = Core.App (Core.TypeApp refVar [hp,tp]) [coreExpr] -- TODO: fragile: depends on order of quantifiers of the ref function!
                               return (Core.Def name refTp refExpr vis sort nameRng doc)

           if (not (isWildcard name)) 
            then addRangeInfo nameRng (RM.Id name (RM.NIValue (Core.defType coreDef)) True)
            else return ()
           return (eff,coreDef)
       

checkValue = Check "Values cannot have an effect"

{--------------------------------------------------------------------------
  Expression
--------------------------------------------------------------------------}
data Expect = Generalized
            | Instantiated
            deriving (Show,Eq)

inferIsolated :: Range -> Range -> Inf (Type,Effect,Core.Expr) -> Inf (Type,Effect,Core.Expr)
inferIsolated contextRange range inf
  = do (tp,eff,core) <- inf
       improve contextRange range eff tp  core

-- | @inferExpr propagated expect expr@ takes a potential propagated type, whether the result is expected to be generalized or instantiated,
-- and the expression. It returns its type, effect, and core expression. Note that the resulting type is not necessarily checked that it matches
-- the propagated type: the propagated type is just a hint (used for example to resolve overloaded names).
inferExpr :: Maybe (Type,Range) -> Expect -> Expr Type -> Inf (Type,Effect,Core.Expr)
inferExpr propagated expect (Lam binders body rng)  
  = -- trace (" inferExpr.Lam: " ++ show propagated ++ ", " ++ show expect) $
    do (propArgs,propEff,propBody,expectBody) <- matchFun propagated
       let binders0 = [case binderType binder of
                         Nothing -> binder{ binderType = fmap snd mbProp }
                         Just _  -> binder
                      | (binder,mbProp) <- zip binders propArgs]
       binders1 <- mapM instantiateBinder binders0
       (infgamma,sub,defs) <- inferOptionals [] binders1        
       let coref c = Core.makeLet (map Core.DefNonRec defs) ((Core.|~>) sub c) 

       returnTp <- case propBody of
                     Nothing     -> Op.freshTVar kindStar Meta
                     Just (tp,_) -> return tp

       (tp,eff,core) <- extendInfGamma False infgamma  $ 
                        extendInfGamma False [(nameReturn,createNameInfoX nameReturn DefVal (getRange body) returnTp)] $
                        inferIsolated rng (getRange body) $ 
                        inferExpr propBody expectBody body
      
       inferUnify (checkReturnResult rng) (getRange body) returnTp tp 
       
       topEff <- case propEff of
                   Nothing -> return eff
                   Just (topEff,r) -> -- trace (" inferExpr.Lam.propEff: " ++ show (eff,topEff)) $
                                      -- inferUnifies (checkEffect rng) [(r,topEff),(getRange body,eff)]
                                      do inferUnify (checkEffectSubsume rng) r eff topEff
                                         return topEff

       parTypes2 <- subst (map binderType binders1)
       let optPars   = zip (map binderName binders1) parTypes2          
           bodyCore1 = (Core.addLambdas optPars (Core.Lam [] (coref core))) -- need empty Lam to ensure a lambda is there if pars is empty
       bodyCore2 <- subst bodyCore1
       let pars = optPars
       (ftp,fcore) <- maybeGeneralize rng (getRange body) typeTotal expect (typeFun pars topEff tp) bodyCore2

       -- check for polymorphic parameters (this has to be done after generalize since some substitution may only exist as a constraint up to that point)
       unannotBinders <- mapM (\b -> do tp <- subst (binderType b); return b{ binderType = tp }) 
                            [b1  | (b0,b1) <- zip binders0 binders1, isNothing (binderType b0)]
       
       -- trace ("unannotBinders: " ++ show tp ++ ": " ++ show [(binderName b, binderType b) | b <- unannotBinders]) $ return ()
       let polyBinders = filter (not . isTau . binderType) unannotBinders
       if (null polyBinders) 
        then return ()
        else let b = head polyBinders
             in typeError (rng) (binderNameRange b) (text "unannotated parameters cannot be polymorphic") (binderType b) [(text "hint",text "annotate the parameter with a polymorphic type")]

       mapM_ (\(binder,tp) -> addRangeInfo (binderNameRange binder) (RM.Id (binderName binder) (RM.NIValue tp) True)) (zip binders1 parTypes2)
       eff <- freshEffect                   
       return (ftp, eff, fcore ) 

inferExpr propagated expect (Let defgroup body rng)
  = do (cgroups,(tp,eff,core)) <- inferDefGroup False defgroup (inferExpr propagated expect body)
       return (tp,eff,Core.Let cgroups core)

inferExpr propagated expect (Bind def body rng)
  = -- trace ("infer bind") $
    do (eff1,coreDef) <- inferBindDef def
       mod  <- getModuleName
       let cgroup = Core.DefNonRec coreDef
       (tp,eff2,coreBody) <- extendInfGammaCore False [cgroup] (inferExpr propagated expect body)
       -- topEff <- addTopMorphisms rng [(defRange def,eff1),(getRange body,eff2)]
       inferUnify (checkEffect rng) (getRange rng) eff1 eff2
       return (tp,eff2,Core.Let [cgroup] coreBody)
        
-- | Return expressions
inferExpr propagated expect (App (Var name _ nameRng) [(_,expr)] rng)  | name == nameReturn
  = do allowed <- isReturnAllowed
       if (False && not allowed) 
        then infError rng (text "illegal expression context for a return statement")
        else  do (tp,eff,core) <- inferExpr propagated expect expr
                 mbTp <- lookupInfName (unqualify nameReturn)
                 case mbTp of
                   Nothing 
                    -> do infError rng (text "illegal context for a return statement")
                   Just (_,retTp)
                    -> do inferUnify (checkReturn rng) (getRange expr) retTp tp
                 resTp <- Op.freshTVar kindStar Meta
                 let typeReturn = typeFun [(nameNil,tp)] typeTotal resTp
                 addRangeInfo nameRng (RM.Id (newName "return") (RM.NIValue tp) False)
                 return (resTp, eff, Core.App (Core.Var (Core.TName nameReturn typeReturn) (Core.InfoExternal [(CS,"return #1"),(JS,"return #1")])) [core])
-- | Assign expression
inferExpr propagated expect (App assign@(Var name _ arng) [lhs@(_,lval),rhs@(_,rexpr)] rng) | name == nameAssign
  = case lval of
      App fun args lrng 
        -> inferExpr propagated expect (App fun (args ++ [(Nothing {- Just (nameAssigned,rangeNull) -},rexpr)]) rng)
      Var target _ lrng
        -> do (_,gtp,_) <- resolveName target Nothing lrng
              (tp,_,_) <- instantiate lrng gtp
              r <- freshRefType
              inferUnify (checkAssign rng) lrng r tp
              inferExpr propagated expect 
                        (App (Var nameRefSet False arng) [(Nothing,App (Var nameByref False (before lrng)) [lhs] lrng), rhs] rng)
              {-  
              (_,_,info) <- resolveName target Nothing lrng
              case info of
                InfoVal{ infoIsVar = True }
                  -> inferExpr propagated expect 
                        (App (Var nameRefSet False lrng) [(Nothing,App (Var nameByref False lrng) [lhs] lrng), rhs] rng)
                _ -> errorAssignable
              -}
      _ -> errorAssignable
  where
    errorAssignable
      = do contextError rng (getRange lval) (text "not an assignable expression") [(text "because",text "an assignable expression must be an application, index expression, or variable")]            
           return (typeUnit,typeTotal,Core.Con (Core.TName (nameTuple 0) typeUnit) (Core.ConSingleton nameTpUnit 0))

    checkAssign 
      = Check "an assignable identifier must have a reference type"

    freshRefType
      = do hvar <- Op.freshTVar kindHeap Meta
           xvar <- Op.freshTVar kindStar Meta
           return (typeApp typeRef [hvar,xvar])
{-
-- | Assign expressions on indexed l-values
inferExpr propagated expect (App assign@(Var name _ _) ((_,App index@(Var iname _ _) iargs _) : args) rng)  | unqualify name == unqualify nameAssign && unqualify iname == unqualify nameIndex
  = inferExpr propagated expect (App assign (iargs ++ args) rng)

-- | Assign expressions on local variables
inferExpr propagated expect (App assign@(Var name _ _) args@[lhs@(_,Var target _ lrng),rhs] rng)  | unqualify name == unqualify nameAssign
  = do (_,_,info) <- resolveName target Nothing lrng
       case info of
        InfoVal{ infoIsVar = True }  
           -> inferExpr propagated expect (App assign [(Nothing,App (Var nameByref False lrng) [lhs] lrng), rhs] rng)
        _  -> inferApp propagated expect assign args rng
-}

-- | Byref expressions
inferExpr propagated expect (App (Var byref _ _) [(_,Var name _ rng)] _)  | byref == nameByref
  = inferVar propagated expect name rng False


-- | Application nodes. Inference is complicated here since we need to disambiguate overloaded identifiers.
inferExpr propagated expect (App fun nargs rng)
  = inferApp propagated expect fun nargs rng

inferExpr propagated expect (Ann expr annTp rng)
  = -- trace (" inferExpr.Ann: " ++ show (pretty annTp)) $
    do (tp,eff,core) <- inferExpr (Just (annTp,rng)) (if isRho annTp then Instantiated else Generalized) expr
       sannTp <- subst annTp
       -- trace (" inferExpr.Ann: subsume annotation: " ++ show (sannTp,tp)) $ return ()
       (resTp0,coref) <- -- withGammaType rng sannTp $ 
                          inferSubsume (checkAnn rng) (getRange expr) sannTp tp
       -- (resTp,resCore) <- maybeInstantiateOrGeneralize expect annTp (coref core)
       -- return (resTp,eff,resCore)
       resTp  <- subst resTp0
       resEff <- subst eff
       resCore <- subst (coref core)
       -- trace ("after subsume: " ++ show (pretty resTp)) $ return ()       
       return (resTp,resEff,resCore)
                           
      

inferExpr propagated expect (Case expr branches rng)
  = -- trace " inferExpr.Case" $
    do (ctp,ceff,ccore) <- allowReturn False $ inferExpr Nothing Instantiated expr
       -- infer branches
       bress <- mapM (inferBranch propagated ctp (getRange expr)) branches
       let (tps,effs,bcores) = unzip3 bress
       -- ensure branches match
       let rngs = map (getRange . branchExpr) branches
           brngs = map getRange branches
       resTp  <- inferUnifyTypes checkMatch (zip tps (zip brngs rngs))
       -- resEff <- addTopMorphisms rng ((getRange expr,ceff):(zip rngs effs))
       {-
       resEff <- inferUnifies (checkEffect rng) ((getRange expr,ceff):(zip rngs effs))
       -}
       resEff <- freshEffect
       mapM_ (\(rng,eff) -> inferUnify (checkEffectSubsume rng) rng eff resEff) ((getRange expr,ceff):(zip rngs effs))
       -- check scrutinee type
       stp <- subst ctp
       if (typeIsCaseLegal stp)
        then return ()
        else typeError rng (getRange expr) (text "can only match on literals or data types") stp []
       -- get data info and analyze branches
       dataInfo <- findDataInfo (getTypeName stp)
       defName  <- currentDefName
       sbcores  <- subst bcores
       let (isTotal,warnings,cbranches) = analyzeBranches sbcores defName rng [dataInfo]
       mapM_ (\(rng,warning) -> infWarning rng warning) warnings
       topEff <- if isTotal
                  then return resEff
                  else -- addTopMorphisms rng [(rng,typePartial),(rng,resEff)]
                       -- return (orderEffect (effectExtend typePartial resEff))
                       -- do subsumeEffect (checkEffectSubsume rng) rng typePartial resEff
                       --   return resEff
                       do sresEff <- subst resEff
                          return (effectExtendNoDup typePartial sresEff)
       -- return core
       core  <- subst (Core.Case [ccore] cbranches)
       (gresTp,gcore) <- maybeInstantiateOrGeneralize rng (getRange branches) resEff expect resTp core
       return (gresTp,topEff,gcore)
  where
    typeIsCaseLegal tp
      = case expandSyn tp of
          TApp (TCon _) _  -> True
          TCon _           -> True
          -- TApp (TVar _) _  -> True
          -- TVar _           -> True
          _                -> False

    getTypeName tp
      = case expandSyn tp of
          TApp (TCon tc) _  -> typeconName tc
          TCon tc           -> typeconName tc
          _                 -> failure ("Type.Infer.inferExpr.Case.getTypeName: not a valid scrutinee? " ++ show tp)
        
    unzip3 xs = unzipx3 [] [] [] xs 
    unzipx3 acc1 acc2 acc3 []           = (reverse acc1, reverse acc2, reverse acc3)
    unzipx3 acc1 acc2 acc3 ((x,y,z):xs) = unzipx3 (x:acc1) (y:acc2) (z:acc3) xs

    inferUnifyTypes contextF [] = matchFailure "Type.Infer.inferExpr.Case.inferUnifyTypes"
    inferUnifyTypes contextF [(tp,_)]  = subst tp
    inferUnifyTypes contextF ((tp1,r):(tp2,(ctx2,rng2)):tps)
      = do inferUnify (contextF ctx2) rng2 tp1 tp2
           inferUnifyTypes contextF ((tp1,r):tps)

inferExpr propagated expect (Var name isOp rng)
  = inferVar propagated expect name rng True

inferExpr propagated expect (Lit lit)
  = do let (tp,core) =
              case lit of
                LitInt i _  -> (typeInt,Core.Lit (Core.LitInt i))
                LitChar c _  -> (typeChar,Core.Lit (Core.LitChar c))
                LitFloat f _  -> (typeFloat,Core.Lit (Core.LitFloat f))
                LitString s _  -> (typeString,Core.Lit (Core.LitString s))
       eff <- freshEffect
       return (tp,eff,core)
       

inferExpr propagated expect (Parens expr rng)
  = inferExpr propagated expect expr
{-
inferExpr propagated expect expr
  = todo ("Type.Infer.inferExpr")
-}

inferApp :: Maybe (Type,Range) -> Expect -> Expr Type -> [(Maybe (Name,Range),Expr Type)] -> Range -> Inf (Type,Effect,Core.Expr)
inferApp propagated expect fun nargs rng
  = -- trace "infer: App" $
    do (fixed,named) <- splitNamedArgs nargs
       amb <- case rootExpr fun of
                (Var name isOp nameRange)
                  -> do matches <- lookupNameN name (length fixed) (map (fst . fst) named) nameRange
                        case matches of
                          []         -> do -- emit an error
                                           resolveFunName name (CtxFunArgs (length fixed) (map (fst . fst) named)) rng nameRange
                                           return (Just Nothing)  -- error
                          [(_,info)] -> return (Just (Just (infoType info, rng))) -- known type
                          _          -> return Nothing -- many matches
                _ -> return (Just Nothing) -- fun first
       case amb of
         Nothing   -> inferAppArgsFirst [] fixed fixed named
         Just prop -> inferAppFunFirst prop fixed named
  where

    -- (names,args) = unzip nargs
    inferAppFunFirst :: Maybe (Type,Range) -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppFunFirst prop fixed named
      = -- trace ("inferAppFunFirst") $
        do -- infer type of function
           (ftp,eff1,fcore)     <- allowReturn False $ inferExpr prop Instantiated fun
           -- match the type with a function type
           (iargs,pars,funEff,funTp,coreApp)  <- matchFunTypeArgs rng fun ftp fixed named

           -- todo: match propagated type with result type?       
           -- subsume arguments
           (effArgs,coreArgs) <- -- withGammaType rng (TFun pars funEff funTp) $ -- ensure the free 'some' types are free in gamma
                                 inferSubsumeN rng (zip (map snd pars) (map snd iargs))
        
           core <- if (monotonic (map fst iargs) || all Core.isTotal coreArgs)
                    then return (coreApp fcore coreArgs)
                    else do -- let bind in evaluation order
                            vars <- mapM (\_ -> uniqueName "arg") iargs
                            let vargs = zip vars [(i,carg) | (carg,(i,_)) <- zip coreArgs iargs]
                                eargs = sortBy (\(_,(i,_)) (_,(j,_)) -> compare i j) vargs
                                defs  = [Core.DefNonRec (Core.Def var (Core.typeOf arg) arg Core.Private DefVal rangeNull "") | (var,(_,arg)) <- eargs]
                                cargs = [Core.Var (Core.TName var (Core.typeOf arg)) Core.InfoNone | (var,(_,arg)) <- vargs]
                            if (Core.isTotal fcore)
                             then return (Core.makeLet defs (coreApp fcore cargs))
                             else do fname <- uniqueName "fun"
                                     let fdef = Core.DefNonRec (Core.Def fname ftp fcore Core.Private DefFun rangeNull "")
                                         fvar = Core.Var (Core.TName fname ftp) Core.InfoNone
                                     return (Core.Let (fdef:defs) (coreApp fvar cargs))
           -- take top effect
           -- todo: sub effecting should add core terms
           -- topEff <- addTopMorphisms rng ((getRange fun, eff1):(rng,funEff):zip (map (getRange . snd) iargs) effArgs)
           topEff <- inferUnifies (checkEffect rng) ((getRange fun, eff1) : zip (map (getRange . snd) iargs) effArgs)
           inferUnify (checkEffectSubsume rng) (getRange fun) funEff topEff
           -- trace (" ** effects: " ++ show (topEff, funEff, eff1, effArgs)) $ return ()

           -- instantiate or generalize result type
           funTp1         <- subst funTp          
           (resTp,resCore) <- maybeInstantiateOrGeneralize rng (getRange fun) topEff expect funTp1 core
           return (resTp,topEff,resCore )

    -- we cannot determine what function is called, infer types of arguments without propagation
    inferAppArgsFirst :: [(Type,(Range,Effect),Core.Expr)] -> [Expr Type] -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf (Type,Effect,Core.Expr)
    inferAppArgsFirst [] [] [] named
      = inferAppFunFirst Nothing [] named
    inferAppArgsFirst acc [] fixed []
      = inferAppFixedArgs acc
    inferAppArgsFirst acc [] fixed named
      = infError rng (text "named arguments can only be used if the function is unambiguously determined by the context" <$> text " hint: annotate the function parameters?" )

    inferAppArgsFirst acc (fix:fixs) fixed named
      = do (tpArg,effArg,coreArg)  <- allowReturn False $ inferExpr Nothing Instantiated fix
           let acc' = (acc ++ [(tpArg,(getRange fix, effArg),coreArg)])
           amb <- case rootExpr fun of
                    (Var name _ nameRange)
                      -> do matches <- lookupNameEx (const True) name (CtxFunTypes True (map fst3 acc') []) nameRange
                            case matches of
                              []         -> do -- emit an error
                                               resolveFunName name (CtxFunTypes True (map fst3 acc') []) rng nameRange 
                                               return Nothing
                              [(_,info)] -> return (Just (infoType info, rng))
                              _          -> return Nothing
                    _ -> return Nothing

           case amb of
             Just prop  -> -- TODO: we re-infer the fixed arguments again. it is hard to optimize this due to optional and delayed arguments that need wrapping...
                           -- what we could do is check if the check arguments up to this point (in acc) are not optional or delayed, and in that case
                           -- we can avoid redoing the inference for those.
                           -- TODO: this can lead to exponential behavior... really bad
                           -- trace(" reinfer") $
                            inferAppFunFirst (Just prop) fixed named
             Nothing    -> {-
                           if (not (null named0)) 
                            then infError rng (text "named arguments can only be used if the function is unambiguously determined by the context" <$> text " hint: annotate the function parameters?" )
                            else do (tpArgs,effArgs,coreArgs)  <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) fixed
                                    inferAppFixedArgs (tpArg1:tpArgs) (zip (map getRange fixed) (effArg1:effArgs)) (coreArg1:coreArgs)
                           -}
                           inferAppArgsFirst acc' fixs fixed named
    {-
    -- lets try again on all arguments
    inferAppArgs fixed named    
      = do (tpArgs,effArgs,coreArgs)    <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) fixed
           (tpNArgs,effNArgs,coreNArgs) <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) (map snd named)
           let tpNamedArgs = zip (map fst named) tpNArgs

           -- inferAppFixedArgs tpArgs (zip (map getRange fixed) effArgs) (coreArgs)

           amb <- case rootExpr fun of
                    (Var name _ nameRange)
                      -> do matches <- lookupNameEx (const True) name (CtxFunTypes False tpArgs [(name,tp) | ((name,_),tp) <- tpNamedArgs]) nameRange
                            case matches of
                              []         -> return Nothing
                              [(_,info)] -> return (Just (infoType info, rng))
                              _          -> return Nothing
                    _ -> return Nothing
           case amb of
             Just prop  -> -- todo: for now, redo all the inference of the arguments to share code, but this could be optimized
                           inferAppFunFirst (Just prop) [] fixed named
             Nothing    -> if (null named)
                            then inferAppFixedArgs tpArgs (zip (map getRange fixed) effArgs) (coreArgs)
                            else infError rng (text "named arguments can only be used if the function is unambiguously determined by the context" <$> text " hint: annotate the function parameters?" )
     -}
     
    inferAppFixedArgs :: [(Type,(Range,Effect),Core.Expr)] -> Inf (Type,Effect,Core.Expr)
    inferAppFixedArgs acc
      = -- trace ("inferAppFixedArgs") $
        do -- (tpArgs,effArgs,coreArgs) <- fmap unzip3 $ mapM (inferExpr Nothing Instantiated) args  -- todo: what about higher-ranked types?
           let (tpArgs,effArgs,coreArgs) = unzip3 acc

           funEff <- freshEffect
           expTp  <- case propagated of
                         Just (tp,_) -> return tp
                         _           -> Op.freshTVar kindStar Meta
           let propType = TFun [(newName "",targ) | targ <- tpArgs] funEff expTp
           (ftp,eff1,fcore) <- allowReturn False $ inferExpr (Just (propType,rng)) Instantiated fun
           -- check the inferred type matches the arguments
           inferUnify (checkFun rng) rng propType ftp
           -- add morphisms
           -- topEff <- addTopMorphisms rng ((getRange fun, eff1):(rng,funEff):effArgs)
           topEff <- inferUnifies (checkEffect rng) ((getRange fun, eff1) : effArgs)
           inferUnify (checkEffectSubsume rng) (getRange fun) funEff topEff

           -- instantiate or generalize result type
           resTp1          <- subst expTp
           (resTp,resCore) <- maybeInstantiateOrGeneralize rng (getRange fun) topEff expect resTp1 (Core.App fcore coreArgs)
           return (resTp,topEff,resCore )

    fst3 (x,y,z) = x

inferVar :: Maybe (Type,Range) -> Expect -> Name -> Range -> Bool -> Inf (Type,Effect,Core.Expr)
inferVar propagated expect name rng isRhs  | isConstructorName name
  = do (qname1,tp1,conRepr,conInfo) <- resolveConName name (fmap fst propagated) rng
       let info1 = InfoCon tp1 conRepr conInfo rng
       (qname,tp,info) <- do defName <- currentDefName
                             let creatorName = newCreatorName qname1
                             -- trace ("inferCon: " ++ show (defName,creatorName,qname1)) $ return ()
                             if (defName /= unqualify creatorName && defName /= nameCopy) -- a bit hacky, but ensure we don't call the creator function inside itself or the copy function
                               then do mbRes <- lookupFunName creatorName propagated rng 
                                       case mbRes of
                                          Just (qname',tp',info') -> return (qname',tp',info')
                                          Nothing  -> return (qname1,tp1,info1)
                               else return (qname1,tp1,info1)
       let coreVar = coreExprFromNameInfo qname info
       addRangeInfo rng (RM.Id (infoCanonicalName qname1 info1) (RM.NICon tp) False)                                  
       (itp,coref) <- maybeInstantiate rng expect tp 
       -- trace ("Type.Infer.Con: " ++ show (name,itp)) $ return ()
       eff <- freshEffect
       return (itp,eff,coref coreVar)                              

inferVar propagated expect name rng isRhs
  = do (qname,tp,info) <- resolveName name propagated rng
       case info of
         InfoVal{ infoIsVar = True }  | isRhs  -- is it a right-hand side variable?
           -> do (tp1,eff1,core1) <- inferExpr propagated expect (App (Var nameDeref False rng) [(Nothing,App (Var nameByref False rng) [(Nothing,Var name False rng)] rng)] rng)
                 addRangeInfo rng (RM.Id qname (RM.NIValue tp1) False)
                 return (tp1,eff1,core1)
         _ -> --  inferVarX propagated expect name rng qname1 tp1 info1
              do let coreVar = coreExprFromNameInfo qname info
                 addRangeInfo rng (RM.Id (infoCanonicalName qname info) (RM.NIValue tp) False)                                  
                 (itp,coref) <- maybeInstantiate rng expect tp 
                 -- trace ("Type.Infer.Var: " ++ show (name,itp)) $ return ()
                 eff <- freshEffect
                 return (itp,eff,coref coreVar)

{-       
inferVar propagated expect name rng isRhs
  = do (qname1,tp1,info1) <- resolveName name (propagated) rng
       inferVarX propagated expect name rng qname1 tp1 info1

inferVarX propagated expect name rng qname1 tp1 info1
  = do (qname,tp,info,rngConValue) 
                      <- case info1 of
                           InfoCon{ infoCon = conInfo } -- conInfoCreator conInfo -- does it have a special creator function?
                            -> do defName <- currentDefName
                                  let creatorName = newCreatorName qname1
                                  -- trace ("inferCon: " ++ show (defName,creatorName,qname1)) $ return ()
                                  if (defName /= unqualify creatorName && defName /= nameCopy) -- a bit hacky, but ensure we don't call the creator function inside itself or the copy function
                                   then do mbRes <- lookupFunName creatorName propagated rng 
                                           case mbRes of
                                              Just (qname',tp',info') -> return (qname',tp',info',RM.NICon)
                                              Nothing  -> return (qname1,tp1,info1,RM.NICon)
                                   else return (qname1,tp1,info1,RM.NICon)
                           _ -> return (qname1,tp1,info1,RM.NIValue)
       let coreVar = coreExprFromNameInfo qname info
       addRangeInfo rng (RM.Id (infoCanonicalName qname1 info1) (rngConValue tp) False)                                  
       (itp,coref) <- maybeInstantiate rng expect tp 
       -- trace ("Type.Infer.Var: " ++ show (name,itp)) $ return ()
       eff <- freshEffect
       return (itp,eff,coref coreVar)
-}

inferBranch :: Maybe (Type,Range) -> Type -> Range -> Branch Type -> Inf (Type,Effect,Core.Branch)
inferBranch propagated matchType matchRange branch@(Branch pattern guard expr)
  = do (pcore,infGamma) <- inferPattern matchType matchRange pattern
       -- infGamma <- extractInfGamma pcore 
       extendInfGamma False infGamma $
        do -- check guard expression
           (gtp,geff,gcore) <- allowReturn False $ inferExpr (Just (typeBool,getRange guard)) Instantiated guard
           inferUnify (checkGuardTotal (getRange branch)) (getRange guard) typeTotal geff
           inferUnify (checkGuardBool (getRange branch)) (getRange guard) typeBool gtp
           -- check branch expression
           (btp,beff,bcore) <- inferExpr propagated Instantiated expr
           resCore <- subst (Core.Branch [pcore] [Core.Guard gcore bcore])
           -- check for unused pattern variables
           let defined = Core.bv pcore
               free    = S.fromList $ map Core.getName $ S.toList $ S.union (Core.fv gcore) (Core.fv bcore)
           case filter (\tname -> not (S.member (Core.getName tname) free)) (Core.tnamesList defined) of
             [] -> return ()
             (name:_) -> do env <- getPrettyEnv
                            infError (getRange pattern) (text "pattern variable" <+> ppName env (Core.getName name) <+> text "is unused (or a wrongly spelled constructor?)" <$>
                                                         text " hint: prepend an underscore to make it a wildcard pattern")
           return (btp,beff,resCore)
  where
    extractInfGamma :: Core.Pattern -> Inf [(Name,Type)]
    extractInfGamma pattern
      = case pattern of
          Core.PatVar (Core.TName name tp) pat -> do stp <- subst tp
                                                     xs  <- extractInfGamma pat
                                                     return ((name,stp) : xs)
          Core.PatCon _ args _ _ _         -> do xss <- mapM extractInfGamma args
                                                 return (concat xss)
          Core.PatWild                     -> return []

inferPattern :: Type -> Range -> Pattern Type -> Inf (Core.Pattern,[(Name,NameInfo)])
inferPattern matchType matchRange (PatCon name patterns0 nameRange range)
  = do (qname,gconTp,repr,coninfo) <- resolveConName name Nothing range
       addRangeInfo nameRange (RM.Id qname (RM.NICon gconTp) False)

       (conTp,tvars,_) <- instantiate range gconTp 
       let (conParTps,conResTp) = splitConTp conTp
       inferUnify (checkConMatch range) nameRange matchType conResTp
       patterns <- matchPatterns range nameRange conTp conParTps patterns0
                   {-
                   if (length conParTps < length patterns0)
                    then do typeError range nameRange (text "constructor has too many arguments") (conTp) []
                            return (take (length conParTps) patterns0)
                    else return (patterns0 ++ (replicate (length conParTps - length patterns0) (Nothing,PatWild range)))        
                   -}
       (cpatterns,infGamma) <- fmap unzip $ mapM (\(parTp,pat) -> 
                                         do sparTp <- subst parTp
                                            inferPattern sparTp matchRange pat) (zip (map snd conParTps) (patterns))                                   
       return (Core.PatCon (Core.TName qname conTp) cpatterns repr (map TVar tvars) coninfo, concat infGamma)
  where
    splitConTp tp
      = case expandSyn tp of
          TFun args eff res -> (args,res)
          res               -> ([],res)
       
inferPattern matchType matchRange (PatVar binder)
  = do {-
       mb <- lookupConName (binderName binder) (binderType binder) (binderNameRange binder)
       case mb of
         Just (qname,conTp,info)
           -- it is actually a constructor
           -> do checkCasing (binderNameRange binder) (binderName binder) qname info
                 inferPattern matchType matchRange (PatCon qname [] (binderNameRange binder) (binderNameRange binder))
         Nothing
           -- it is a variable indeed         
           -> -}
              do addRangeInfo (binderNameRange binder) (RM.Id (binderName binder) (RM.NIValue matchType) True)
                 case (binderType binder) of
                   Just tp -> inferUnify (checkAnn (getRange binder)) (binderNameRange binder) matchType tp 
                   Nothing -> return ()
                 (cpat,infGamma) <- inferPattern matchType matchRange (binderExpr binder)
                 return (Core.PatVar (Core.TName (binderName binder) matchType) cpat, [(binderName binder,(createNameInfoX (binderName binder) DefVal (binderNameRange binder) matchType))] ++ infGamma)

inferPattern matchType matchRange (PatWild range)
  = return (Core.PatWild,[])

inferPattern matchType matchRange (PatAnn pat tp range)
  = do res <- inferPattern tp range pat
       inferUnify (checkAnn range) (getRange pat) matchType tp  -- TODO: improve error message
       return res

inferPattern matchType matchRange (PatParens pat range)
  = inferPattern matchType matchRange pat       
{-
inferPattern matchType matchRange pattern
  = todo ("Type.Infer.inferPattern")
-}

-- | Infer automatic unwrapping for parameters with default values, and adjust their type from optional<a> to a
-- Takes an accumulated InfGamma (initially empty), a list of parameters (as value binders) and returns
-- the new InfGamma and a substitution from optional paramter names to the local unique names (of type a)
-- and a list of core (non-recursive) bindings (where the substitution has already been applied)
inferOptionals :: [(Name,NameInfo)] -> [ValueBinder Type (Maybe (Expr Type))] -> Inf ([(Name,NameInfo)],[(Core.TName,Core.Expr)],[Core.Def])
inferOptionals infgamma []
  = return (infgamma,[],[])
inferOptionals infgamma (par:pars)
  = case binderExpr par of
     Nothing 
      -> inferOptionals (infgamma ++ [(binderName par,createNameInfoX (binderName par) DefVal (getRange par) (binderType par))]) pars

     Just expr  -- default value
      -> do let fullRange = combineRanged par expr
                -- partp = binderType par
                -- optTp = makeOptional partp
                optTp = binderType par
            
            -- infer parameter type from optional
            tvar <- Op.freshTVar kindStar Meta
            inferUnify (Infer fullRange) (getRange par) optTp (makeOptional tvar)
            partp <- subst tvar

            -- infer expression      
            (exprTp,exprEff,coreExpr) <- extendInfGamma False infgamma $ inferExpr (Just (partp,getRange par)) (if isRho partp then Instantiated else Generalized) expr
            inferUnify (checkOptional fullRange) (getRange expr) partp exprTp
            inferUnify (checkOptionalTotal fullRange) (getRange expr) typeTotal exprEff
            tp <- subst partp
            let infgamma' = infgamma ++ [(binderName par,createNameInfoX (binderName par) DefVal (getRange par) tp)]
            
            -- build up core to get the optional value
            local <- uniqueName (show (binderName par))
            temp  <- uniqueName (show (binderName par))
            -- let coreVar (qname,tp,info) = Core.Var (Core.TName qname tp) (coreVarInfoFromNameInfo info)
            dataInfo <- findDataInfo nameTpOptional
            (coreNameOpt,coreTpOpt,coreReprOpt,conInfoOpt) <- resolveConName nameOptional Nothing fullRange
            let tempName = Core.TName temp tp
            let parName  = Core.TName (binderName par) optTp
                corePar = Core.Var parName Core.InfoNone
                init = Core.Case [corePar]
                       [  Core.Branch [ Core.PatCon (Core.TName coreNameOpt coreTpOpt)
                                                    [Core.PatVar tempName Core.PatWild] 
                                                    (coreReprOpt)
                                                    [tp]
                                                    conInfoOpt
                                      ] 
                                      [ Core.Guard   Core.exprTrue (Core.Var tempName Core.InfoNone) ]
                       ,  Core.Branch [ Core.PatWild ]
                                      [ Core.Guard   Core.exprTrue coreExpr ]
                       ] 
                def  = Core.Def local partp init Private DefVal (binderNameRange par) ""
                sub  = [(Core.TName (binderName par) tp, Core.Var (Core.TName local partp) Core.InfoNone)]
                -- coref core
                --   = Core.Let [Core.DefNonRec def] ((Core.|~>) sub core)
            
            -- infer the rest
            (infgamma2,sub2,defs2) <- inferOptionals infgamma' pars
            return (infgamma2,sub ++ sub2,def : ((Core.|~>) sub defs2))
       

checkFun        = Check "function type does not match the argument types"
checkMatch      = Check "branch has not the same type as previous branches"
checkAnn        = Check "type cannot be instantiated to match the annotation"
checkRec        = Check "recursive invocations do not match the assumed type; add a type annotation?"
checkGuardTotal = Check "guard expressions must be total"
checkGuardBool  = Check "guard expressions must be of a boolean type"
checkConMatch   = Check "constructor must have the same the type as the matched term"
checkLit        = Check "literal does not match the expected type"
checkOptional   = Check "default value does not match the parameter type"
checkOptionalTotal = Check "default value expression must be total"

checkEffect        = Infer
checkEffectSubsume = Check "effect cannot be subsumed"

checkReturnResult  = Check "function returns values of different types"
checkReturn        = Check "return type does not match an earlier return type"

isAmbiguous :: NameContext -> Expr Type -> Inf Bool
isAmbiguous ctx expr
  = case rootExpr expr of
      (Var name isOp nameRange)
        -> do matches <- lookupNameEx (const True) name ctx nameRange
              case matches of
                []  -> return False
                [_] -> return False
                _   -> return True
      _ -> return False


rootExpr expr
  = case expr of
      -- Let defs e _ -> rootExpr e
      -- Bind def e _ -> rootExpr e
      -- Ann e t r  -> rootExpr e  -- better to do FunFirst in this case
      Parens e r -> rootExpr e
      _          -> expr





coreVarInfoFromNameInfo :: NameInfo -> Core.VarInfo
coreVarInfoFromNameInfo info
  = case info of
      InfoVal _ tp _ _           -> Core.InfoNone
      InfoFun _ tp (m,n) _       -> Core.InfoArity m n
      InfoExternal _ tp format _ -> Core.InfoExternal format
      _                          -> matchFailure "Type.Infer.coreVarInfoFromNameInfo"

coreExprFromNameInfo qname info
  = -- trace ("create name: " ++ show qname) $
    case info of
      InfoVal cname tp _ _            -> Core.Var (Core.TName cname tp) (Core.InfoNone)
      InfoFun cname tp ((m,n)) _      -> Core.Var (Core.TName cname tp) (Core.InfoArity m n)
      InfoCon tp repr _ _             -> Core.Con (Core.TName qname tp) repr
      InfoExternal cname tp format _  -> Core.Var (Core.TName cname tp) (Core.InfoExternal format)
      InfoImport _ _ _ _              -> matchFailure "Type.Infer.coreExprFromNameInfo"

{--------------------------------------------------------------------------
  inferSubsumeN
--------------------------------------------------------------------------}

-- | Infer types of function arguments
inferSubsumeN :: Range -> [(Type,Expr Type)] -> Inf ([Effect],[Core.Expr])
inferSubsumeN range parArgs
  = do res <- inferSubsumeN' range [] (zip [1..] parArgs)
       return (unzip res)

inferSubsumeN' range acc []
  = return (map snd (sortBy (\(i,_) (j,_) -> compare i j) acc))
inferSubsumeN' range acc parArgs
  = do ((i,(tpar,arg)):rest) <- pickArgument parArgs
       (targ,teff,core) <- allowReturn False $ inferExpr (Just (tpar,getRange arg)) (if isRho tpar then Instantiated else Generalized) arg
       tpar1  <- subst tpar
       (_,coref)  <- if isAnnot arg
                      then do inferUnify (Infer range) (getRange arg) tpar1 targ
                              return (tpar1,id)
                      else inferSubsume (Infer range) (getRange arg) tpar1 targ           
       rest1 <- mapM (\(j,(tpar,arg)) -> do{ stpar <- subst tpar; return (j,(stpar,arg)) }) rest
       teff1 <- subst teff
       inferSubsumeN' range ((i,(teff1,coref core)):acc) rest1

-- | Pick an argument that can be subsumed unambigiously..
-- split arguments on non-ambiguous variables and ambiguous ones
-- then we look for annotated arguments since their type is fully determined (and need no type propagation)
-- finally we look at parameters that are not a type variable since those are unambigiously determined (and benefit from type propagation)
-- and finally, we do the ones that are left over (the order of those does not matter)
pickArgument args
  = do ambs <- mapM (\(i,(tpar,arg)) -> isAmbiguous (CtxType tpar) arg) args
       let (ambargs,args0) = partition fst (zip ambs args)
           (annots,args1)  = partition (\(i,(tp,arg)) -> isAnnot arg) (map snd args0)
           (nonvars,args2) = partition (\(i,(tp,arg)) -> not (isTVar tp)) args1
       return (annots ++ nonvars ++ args2 ++ map snd ambargs)

-- | Is an expression annotated?
isAnnot (Parens expr rng)     = isAnnot expr
isAnnot (Ann expr tp rng)     = True
isAnnot (Let defs body rng)   = isAnnot body
isAnnot (Bind defs body rng)  = isAnnot body
isAnnot _                     = False



splitNamedArgs :: Ranged e => [(Maybe (Name,Range),e)] -> Inf ([e],[((Name,Range),e)])
splitNamedArgs nargs
  = do let (nfixed,rest)      = span (isNothing . fst) nargs
           (nnamed,morefixed) = span (not . isNothing . fst) rest
           fixed              = map snd nfixed
           named              = [((name,rng),expr) | (Just (name,rng),expr) <- nnamed]
       -- check that named arguments all come after the positional ones
       case morefixed of
         [] -> return ()
         (arg:_) -> infError (getRange (snd arg)) (text "positional arguments cannot follow named arguments")
       checkDuplicates [] named
       
       return (fixed,named)
  where
    checkDuplicates seen named
      = case named of
          [] -> return ()
          (((name,rng),_):named)
            -> if (name `elem` seen)
                then do env <- getPrettyEnv
                        infError rng (text "named argument" <+> ppName env name <+> text "is given more than once")
                else checkDuplicates (name:seen) named

isNothing Nothing = True
isNothing _       = False


matchPatterns :: Range -> Range -> Type -> [(Name,Type)] -> [(Maybe (Name,Range),Pattern Type)] -> Inf [Pattern Type]
matchPatterns context nameRange conTp conParTypes patterns0
  = do patterns1 <- if (length conParTypes < length patterns0)
                     then do typeError context nameRange (text "constructor has too many arguments") (conTp) []
                             return (take (length conParTypes) patterns0)         
                     else return patterns0

       (fixed,named) <- splitNamedArgs patterns1
       (pars1,pats1) <- matchFixed (zip [0..] conParTypes) fixed
       ipats2        <- matchNamed pars1 named
       return (pats1 ++ map snd (sortBy (\(i,_) (j,_) -> compare i j) ipats2))
  where
    matchFixed :: [(Int,(Name,Type))] -> [Pattern Type] -> Inf ([(Int,(Name,Type))],[Pattern Type])
    matchFixed pars pats
      = return (drop (length pats) pars, pats)

    matchNamed :: [(Int,(Name,Type))] -> [((Name,Range),Pattern Type)] -> Inf [(Int,Pattern Type)]
    matchNamed [] []
      = return []
    matchNamed [] (((_,rng),pat):_)
      = do -- typeError context (combineRanged rng pat) (text "constructor is given too many arguments") tp []
           return []
    matchNamed pars (((name,rng),pat):named)
      = case remove name [] pars of
          Nothing -> do typeError context rng (text "there is no constructor field with name" <+> pretty name) conTp []
                        matchNamed pars named
          Just (i,pars1)
              -> do rest <- matchNamed pars1 named
                    return ((i,pat):rest)      
    matchNamed pars []
      = return [(i,PatWild context) | (i,_) <- pars]

    remove name acc []
      = Nothing
    remove name acc (par@(i,(parName,parType)):pars)
      = if (name == parName)
         then Just (i,reverse acc ++ pars)
         else remove name (par:acc) pars 
         

matchFunTypeArgs :: Range -> Expr Type -> Type -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf ([(Int,Expr Type)],[(Name,Type)], Effect, Type,Core.Expr -> [Core.Expr] -> Core.Expr)
matchFunTypeArgs context fun tp fixed named
  = case tp of
       TFun pars eff res   -> do args <- matchParameters pars fixed named
                                 -- trace ("matched parameters: " ++ show (pars,map fst args)) $
                                 return (args,pars,eff,res,Core.App)
       TSyn _ _ t          -> matchFunTypeArgs context fun t fixed named
       TVar tv             -> do if (null named)
                                  then return ()
                                  else infError range (text "cannot used named arguments on an inferred function" <$> text " hint: annotate the parameters")
                                 targs <- mapM (\name -> do{ tv <- Op.freshTVar kindStar Meta; return (name,tv)}) ([nameNil | a <- fixed] ++ map (fst . fst) named)
                                 teff  <- Op.freshTVar kindEffect Meta
                                 tres  <- Op.freshTVar kindStar Meta
                                 -- trace ("Type.matchFunType: " ++ show tv ++ ": " ++ show (targs,teff,tres)) $
                                 extendSub (subSingle tv (TFun targs teff tres))
                                 return (zip [0..] (fixed ++ map snd named), targs,teff,tres,Core.App)
       _  -> do -- apply the copy constructor if we can find it
                matches <- lookupNameEx (const True) nameCopy (CtxFunTypes True [tp] []) range 
                case matches of
                  [(qname,info)]
                    -> do (contp,_,coreInst) <- instantiate range (infoType info)
                          (args,pars,eff,res,_) <- matchFunTypeArgs context fun contp (fun:fixed) named
                          let coreAddCopy core coreArgs
                                = let coreVar = coreExprFromNameInfo qname info
                                  in (Core.App (coreInst coreVar) (coreArgs))
                          return (args,pars,eff,res,coreAddCopy)
                  _ -> do typeError context range (text "only functions or types with a copy constructor can be applied") tp []
                          return (zip [1..] (fixed ++ map snd named), [], typeTotal, typeUnit, Core.App)
  where
    range = getRange fun

    matchParameters :: [(Name,Type)] -> [Expr Type] -> [((Name,Range),Expr Type)] -> Inf [(Int,Expr Type)]
    matchParameters pars fixed named
      = -- trace ("match parameters: " ++ show (pars,length fixed,map (fst.fst) named)) $
        do (pars1,args1) <- matchFixed pars (zip [0..] fixed)
           iargs2        <- matchNamed (zip [length fixed..] pars1) (zip [length fixed..] named)
           return (args1 ++ map snd (sortBy (\(i,_) (j,_) -> compare i j) iargs2))

    matchFixed :: [(Name,Type)] -> [(Int,Expr Type)] -> Inf ([(Name,Type)],[(Int,Expr Type)])
    matchFixed pars [] 
      = return (pars,[])
    matchFixed ((name,tp):pars) ((i,arg):fixed)
      = do newarg <- if (isOptional tp) 
                      then return (wrapOptional arg)
                     else if (isDelay tp)
                      then wrapDelay arg
                      else return arg
           (prest,rest) <- matchFixed pars fixed 
           return (prest, (i,newarg):rest)
    matchFixed [] ((i,arg):_)
      = do typeError context (getRange fun) (text "function is applied to too many arguments") tp []
           return ([],[])

    -- in the result, the first int is position of the parameter, the second int it the original position of
    -- the argument (so we can evaluate in argument order)
    matchNamed :: [(Int,(Name,Type))] -> [(Int,((Name,Range),Expr Type))] -> Inf [(Int,(Int,Expr Type))]
    matchNamed [] [] 
      = return []
    matchNamed [] ((i,((name,rng),arg)):named)
      = do typeError context (getRange fun) {- (combineRanged rng arg) -} (text "function is applied to too many arguments") tp []
           return []
    matchNamed pars ((i,((name,rng),arg)):named)
      = case extract name [] pars of
          Nothing -> do typeError context (getRange fun) (text "there is no parameter with name" <+> pretty name) tp []
                        matchNamed pars named
          Just (j,tp,pars1)
              -> do newarg  <- if (isOptional tp) 
                                then return (wrapOptional arg)
                               else if (isDelay tp)
                                then wrapDelay arg
                                else return arg
                    rest <- matchNamed pars1 named
                    return ((j,(i,newarg)):rest)      
    matchNamed pars []
      = do if (all (isOptional . snd . snd) pars) 
            then return [(j,(i,makeOptionalNone)) | (i,(j,(name,tpar))) <- zip [(length fixed + length named)..] pars]
            else do typeError context range (text "function has not enough arguments") tp []
                    return []

    extract name acc []
      = Nothing
    extract name acc (par@(i,(parName,parType)):pars)
      = if (name == parName)
         then Just (i,parType,reverse acc ++ pars)
         else extract name (par:acc) pars 
      
    wrapOptional :: Expr Type -> Expr Type
    wrapOptional expr
      = App (Var nameOptional False (rangeNull {- getRange expr -}))  -- use a null range so it doesn't show in the documentation
            [(Nothing,expr)] (getRange expr)
  
    wrapDelay :: Expr Type -> Inf (Expr Type)
    wrapDelay expr
      = do delayed <- isDelayed expr
           if delayed
            then return expr
            else return (Lam [] expr (getRange expr))
      where
        isDelayed expr
          = case expr of
              Lam [] _ _   -> return True
              Var name _ _ -> do matches <- lookupNameEx (const True) name (CtxFunArgs 0 []) (getRange expr)
                                 case matches of
                                   [(_,info)] -> return (isDelayedType (infoType info))
                                   _          -> return False
              _            -> return False
        
        isDelayedType tp
          = case expandSyn tp of
              TFun [] _ _ -> True
              _           -> False

    makeOptionalNone :: Expr Type
    makeOptionalNone
      = Var nameOptionalNone False rangeNull

    isDelay :: Type -> Bool
    isDelay tp
      = case tp of
          TSyn syn [_,_] _ -> (typesynName syn == nameTpDelay)
          _ -> False

{--------------------------------------------------------------------------
  Effects
--------------------------------------------------------------------------}
{-
addTopMorphisms :: Range -> [(Range,Effect)] -> Inf Effect
addTopMorphisms range effs0
  = -- trace ("add morphisms: " ++ show (map snd effs0)) $
    do effs1 <- subst effs0
       topEffects range (filter (not . isTypeTotal) (map snd effs1))
-}



freshEffect :: Inf Effect
freshEffect
  = Op.freshTVar kindEffect Meta


{--------------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------------}
instantiateBinder :: ValueBinder (Maybe Type) (Maybe (Expr Type)) -> Inf (ValueBinder Type (Maybe (Expr Type)))
instantiateBinder binder
  = do tp <- case binderType binder of
              Just tp -> return tp
              Nothing -> Op.freshTVar kindStar Meta
       return binder{ binderType = tp }
  
maybeGeneralize :: Range -> Range -> Effect -> Expect -> Rho -> Core.Expr-> Inf (Scheme,Core.Expr)
maybeGeneralize contextRange range eff expect tp core
  = case expect of
      Instantiated -> return (tp,core)
      Generalized  -> generalize contextRange range eff tp core

maybeInstantiate :: Range -> Expect -> Scheme -> Inf (Rho,Core.Expr -> Core.Expr)
maybeInstantiate range expect tp 
  = case expect of
      Generalized  -> return (tp,id)
      Instantiated -> do (rho,_,coref) <- instantiate range tp 
                         return (rho,coref)


maybeInstantiateOrGeneralize :: Range -> Range -> Effect -> Expect -> Type -> Core.Expr -> Inf (Type,Core.Expr)
maybeInstantiateOrGeneralize contextRange range eff expect tp core
  = case expect of
      Generalized  -> generalize contextRange range eff tp core
      Instantiated -> do (tp,_,coref) <- instantiate range tp 
                         return (tp, coref core)


-- | Try to match the propagated type with a function type, 
-- returning the propagated argument, effect, and result type, and the expected instantiation of the result
matchFun :: Maybe (Type,Range) -> Inf ([Maybe (Name,Type)],Maybe (Type,Range), Maybe (Type,Range) , Expect)
matchFun mbType
  = case mbType of
      Nothing       -> return (repeat Nothing,Nothing,Nothing,Instantiated)
      Just (tp,rng) -> do (rho,_,_) <- instantiate rng tp 
                          case splitFunType rho of
                           Nothing -> return (repeat Nothing,Nothing,Nothing,Instantiated)
                           Just (args,eff,res) 
                            -> return (map Just args ++ repeat Nothing, Just (eff,rng), Just (res,rng), if isRho res then Instantiated else Generalized)

monotonic :: [Int] -> Bool
monotonic []  = True
monotonic [i] = True
monotonic (i:j:xs) = (i < j && monotonic (j:xs))

before range
  = makeRange (rangeStart range) (rangeStart range)


find :: Range -> M.Map Range a -> a
find range rm
  = case M.lookup range rm of
      Just x -> x
      Nothing -> failure ("Type.Infer.find: could not find: " ++ show range)