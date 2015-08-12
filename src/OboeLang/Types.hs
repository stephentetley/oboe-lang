{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Types
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Syntax for representing types.
-- 
-- Note - there is no real type checking!
--
--------------------------------------------------------------------------------

module OboeLang.Types
  (

    Type(..)

  ) where

import OboeLang.Utils.Pretty

import Text.PrettyPrint.HughesPJ                -- package: pretty


--
-- Lists are used to represent - rest-of-args to opcode calls,
-- they cannot be formed in let- bindings, do-binds ...
--
-- Tuples can only be formed in do-binds e.g. for stereo signals.
--
data Type = TyNull
          | TyInt
          | TyFloat
          | TyString
          | TyAny
          | TyASig
          | TyKSig
          | TyISig
          | TyKA
          | TyFun   [Type]  Type
          | TyTuple [Type]
          | TyList  [Type]      
          | TyCsMon Type
  deriving (Eq,Show)

instance Pretty Type where
  pretty (TyNull)         = text "()"
  pretty (TyInt)          = text "Int"
  pretty (TyFloat)        = text "Float"
  pretty (TyString)       = text "String"
  pretty (TyAny)          = text "Any"
  pretty (TyASig)         = text "ASig"
  pretty (TyKSig)         = text "KSig"
  pretty (TyISig)         = text "ISig"
  pretty (TyKA)           = text "KA"
  pretty (TyFun args ans) = 
    braces (commaSep $ map pretty args) <+> text "->" <+> pretty ans

  pretty (TyTuple tys)    = parens (commaSep $ map pretty tys)
  pretty (TyList tys)     = brackets (commaSep $ map pretty tys)
  pretty (TyCsMon ty)     = text "Cs" <+> pretty ty


