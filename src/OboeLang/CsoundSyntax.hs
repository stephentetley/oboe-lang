{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.CsoundSyntax
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Csound syntax - lowest level.
--
--------------------------------------------------------------------------------

module OboeLang.CsoundSyntax
  (

    Program(..)
  , Instrument(..)
  , Stmt(..)
  , PrimCall(..)
  , CompValue(..)
  , VarName
  , Value(..)


  ) where


import OboeLang.CsoundExprPretty
import OboeLang.SyntaxCommon hiding ( unaryDocE, binDocE, relDocE )
import OboeLang.Utils.Common
import OboeLang.Utils.Pretty
import OboeLang.Utils.PrettyExprHPJ


import Text.PrettyPrint.HughesPJ hiding ( TextDetails(..) )   -- package: pretty

import Data.List ( intersperse )

-- | At some point may re-introduce global statements...
--
data Program = Program
    { prgm_instrs       :: [Instrument]
    }
  deriving (Eq,Show)


data Instrument = Instrument
      { instr_num       :: Int
      , instr_body      :: [Stmt]
      }
  deriving (Eq,Show)


type CodeBlock = [Stmt]

data Stmt  = AssignE VarId Value                   -- lvar = expr
           | Assign1 VarId PrimCall
           | Assign2 VarId VarId PrimCall 
           | Assign3 VarId VarId VarId PrimCall
           | Assign4 VarId VarId VarId VarId PrimCall
           | OpCall PrimCall                 -- no assign, e.g. out
           | Ifte CompValue CodeBlock CodeBlock
  deriving (Eq,Show)



data PrimCall = PrimCall Ident [Value]
  deriving (Eq,Show)

-- Comparision value 
data CompValue = CompValue RelOp Value Value
  deriving (Eq,Show)


type VarName = String


-- | Values can be passed as arguments to Opcode calls. Hence 
-- the set of values includes unary, binary and relational
-- /expressions/.
--
-- The CondE (from Syntax) constructor needs to be eliminated.
--
data Value = Int        Int
           | Float      Decimal
           | String     String
           | Var        VarName
           | Pfield     Int
           | UnaryV     UnaryOp     Value
           | BinV       BinOp       Value   Value
           | CondV      CompValue   Value   Value
           | FunCallV   Ident       [Value]
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Pretty print


instance Pretty Program where
  pretty (Program {prgm_instrs = insts})  = 
    vcat $ map (\i -> pretty i $+$ blank) insts


instance Pretty Instrument where
  pretty (Instrument i body) = 
        text "instr" <+> int i
    $+$ (vcat $ map pretty body)
    $+$ text "endin"

instance Pretty Stmt where
  pretty (AssignE vid v)          = 
    padTextR 11 (varName vid) <+> char '=' <+> pretty v

  pretty (Assign1 vid p)          = 
    padTextR 11 (varName vid) <+> pretty p

  pretty (Assign2 v1 v2 p)        = padTextR 11 ss <+> pretty p
    where
      ss = varName v1 ++ "," ++ varName v2

  pretty (Assign3 v1 v2 v3 p)     = padTextR 11 ss <+> pretty p
    where
      ss = varName v1 ++ "," ++ varName v2 ++ "," ++ varName v3

  pretty (Assign4 v1 v2 v3 v4 p)  = padTextR 11 ss <+> pretty p
    where
      ss = concat $ intersperse "," $ map varName [v1,v2,v3,v4]

  pretty (OpCall p)               = padTextR 11 "" <+> pretty p

  pretty (Ifte cv b1 b2)          = 
    padTextR 11 "" <+> text "if" <+> parens (pretty cv) 
                   <+> text "then"
                   $+$ nest 2 (vcat $ map pretty b1)
                   $+$ text "else"
                   $+$ nest 2 (vcat $ map pretty b2)
                   $+$ text "endif"


instance Pretty PrimCall where
  pretty (PrimCall pid es) = pretty pid <+> commaSep (map pretty es)


instance Pretty CompValue where
  pretty (CompValue op v1 v2) = 
    unparse $ relDocE op (exprValue v1) (exprValue v2)

instance Pretty Value where
  pretty = unparse . exprValue

exprValue :: Value -> DocE
exprValue (Int i)                   = Atom $ int i
exprValue (Float d)                 = 
    Atom $ text $ truncatedDouble $ realToFrac d

exprValue (String s)                = Atom $ doubleQuotes $ text s
exprValue (Var v)                   = Atom $ text v
exprValue (Pfield i)                = Atom $ text $ 'p' : show i
exprValue (UnaryV op v)             = unaryDocE op (exprValue v)
exprValue (BinV op v1 v2)           = binDocE op (exprValue v1) (exprValue v2)
exprValue (CondV cv v1 v2)          = 
    Atom $ parens (pretty cv <+> char '?'   <+> pretty v1
                             <+> char ':'   <+> pretty v2 )

exprValue (FunCallV pid vs)         = 
    Atom $ pretty pid <> tupled (map pretty vs)


