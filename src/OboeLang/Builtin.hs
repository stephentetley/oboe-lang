{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Builtin
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Syntax for representing Csound builtins.
--
--------------------------------------------------------------------------------

module OboeLang.Builtin
  (
    CsPrimName
  , Builtin(..)

  , BuiltinDefn(..)
  , BuiltinDict
  , makeBuiltinDict

  ) where

import OboeLang.SyntaxCommon
import OboeLang.Types
import OboeLang.Utils.Pretty

import Text.PrettyPrint.HughesPJ                -- package: pretty


import qualified Data.Map as Map

type CsPrimName = String

data Builtin = Builtin Ident CsPrimName Type
  deriving (Eq,Show)

instance Pretty Builtin where
  pretty (Builtin vid ss ty) = 
    pretty vid <+> text ":" <+> doubleQuotes (text ss) <+> text "::" <+> pretty ty


data BuiltinDefn = BuiltinDefn CsPrimName Type
  deriving (Eq,Show)


type BuiltinDict = Map.Map Ident BuiltinDefn


makeBuiltinDict :: [Builtin] -> BuiltinDict
makeBuiltinDict = foldr fn Map.empty 
  where
    fn (Builtin vid ss ty) = Map.insert vid (BuiltinDefn ss ty)


