-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Primitive names.
-}
-----------------------------------------------------------------------------
module Common.NamePrim
          ( 
          -- * Interpreter
            nameExpr, nameMain, nameType
          , nameInteractive, nameInteractiveModule
          , nameSystemCore
--          , nameCore
          , nameOpExpr

          -- * Operations
          , namePatternMatchError, nameMainConsole
          , nameCopy
          , nameAssign, nameRefSet, nameAssigned
          , nameByref, nameDeref, nameIndex
          , nameSubStr1

          , nameUnit
          , nameReturn

          -- * Constructors
          , nameTrue, nameFalse
          , nameOptional, nameOptionalNone
          , nameTpDelay
          -- * Lists
          , nameNull, nameCons, nameEnumFromTo, nameEnumFromThenTo, nameTpList
          -- * Type constructors
          , nameEffectEmpty, nameEffectExtend
          , nameEffectAppend

          , nameTpBool, nameTpInt, nameTpChar
          , nameTpFloat
          , nameTpString
          , nameTpAny
          , nameTpAsync
          , nameTpException
          , nameTpMDict, nameTpDict, nameTpBuilder

          , nameTpUnit, nameTpVoid
          , nameTpRef, nameRef
          , nameTpOptional
          , nameTpArray, nameTpVector

          , nameTpTotal, nameTpDiv, nameTpPartial, nameTpPure
          , nameTpST
          , nameTpWrite, nameTpRead
          , nameTpIO
          , nameTpAlloc
          
          , nameTuple, isNameTuple

          , namePredHeapDiv

          -- * Kind constructors
          , nameKindStar, nameKindFun
          , nameKindLabel
          , nameKindPred, nameKindEffect
          , nameKindHeap

          , toShortModuleName

          , namesSameSize
          ) where

import Common.Name



{--------------------------------------------------------------------------
  Special
--------------------------------------------------------------------------}
nameExpr        = newName ".expr"
nameType        = newName ".type"

nameInteractiveModule  = newName "interactive"
nameInteractive = newName "interactive"
nameMain        = newName ".main"
nameCopy        = newName ".copy"
nameOpExpr      = newName ".opexpr"

{--------------------------------------------------------------------------
  Primitive operations
--------------------------------------------------------------------------}
nameIf          = newName "if"
nameCase        = newName "case"
nameUnit        = newName "()"

namePredHeapDiv :: Name
namePredHeapDiv = preludeName "hdiv"

nameReturn :: Name
nameReturn = preludeName ".return"

{--------------------------------------------------------------------------
  Primitive constructors
--------------------------------------------------------------------------}
nameTrue        = preludeName "True"
nameFalse       = preludeName "False"

nameOptional         = preludeName "Optional"
nameOptionalNone     = preludeName "None"
nameTpOptional       = preludeName "optional"

nameTpDelay          = preludeName "delay"

namePatternMatchError = preludeName "patternMatchError"
nameMainConsole      = preludeName "mainConsole"
nameSubStr1          = preludeName "substr1"

nameAssign      = preludeName ":="
nameAssigned    = newName "assigned"
nameRefSet      = preludeName "set"

nameDeref       = preludeName "!"
nameByref       = preludeName ".&"
nameIndex       = newName "[]"

nameTpArray     = preludeName "array"
nameTpVector    = preludeName "vector"

namesSameSize   = map preludeName ["id","map","reverse","foldl","foldr"]

{--------------------------------------------------------------------------
  Lists
--------------------------------------------------------------------------}
nameNull        = preludeName "Nil"
nameCons        = preludeName "Cons"
nameEnumFromTo  = preludeName "enumFromTo"
nameEnumFromThenTo = preludeName "enumFromThenTo"
nameTpList      = preludeName "list"



{--------------------------------------------------------------------------
  Primitive type constructors
--------------------------------------------------------------------------}
nameEffectEmpty = preludeName "<>"
nameEffectExtend= preludeName "<|>"
nameEffectAppend= newName ".<+>"

nameTpBool      = preludeName "bool"
nameTpInt       = preludeName "int"
nameTpFloat     = preludeName "double"
nameTpChar      = preludeName "char"
nameTpString    = preludeName "string"
nameTpAny       = preludeName "any"

nameTpIO        = preludeName "io"
nameTpUnit      = preludeName "()"
nameTpRef       = preludeName "ref"
nameRef         = preludeName "ref"

nameTpTotal     = preludeName "total"
nameTpPartial   = preludeName "exn"
nameTpDiv       = preludeName "div"
nameTpPure      = preludeName "pure"


nameTpAlloc        = preludeName "alloc"
nameTpRead         = preludeName "read"
nameTpWrite        = preludeName "write"
nameTpST           = preludeName "st"

nameTpVoid      = preludeName "void"

nameTpAsync     = preludeName "async"
nameTpException = preludeName "exception"

nameTpMDict     = qualify nameDict (newName "mdict")
nameTpDict      = qualify nameDict (newName "dict")
nameTpBuilder   = qualify (newName "std/string") (newName "builder")

nameTuple :: Int -> Name
nameTuple n     = preludeName ("(" ++ (replicate (n-1) ',') ++ ")")

isNameTuple :: Name -> Bool
isNameTuple name
  = nameModule name == nameId nameSystemCore && length s >= 2 && head s == '(' && last s == ')' && all (==',') (tail (init s))
  where
    s = nameId name

preludeName s
  = qualify nameSystemCore (newName s)

nameSystemCore  = newName "std/core"
nameCore        = newName "core"
nameDict        = newName "std/dict"

toShortModuleName :: Name -> Name
toShortModuleName name
  = let short = last (splitModuleName name) in
    if (short == nameCore) then nameSystemCore else short  -- so primitives can be qualified correctly
    
{--------------------------------------------------------------------------
  Primitive kind constructors
--------------------------------------------------------------------------}
nameKindStar    = newName "V"
nameKindLabel   = newName "X"
nameKindFun     = newName "->"
nameKindPred    = newName "P"
nameKindEffect  = newName "E"
nameKindHeap    = newName "H"
