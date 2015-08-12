{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ScSyntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- SuperCollider syntax - lowest level.
--
--------------------------------------------------------------------------------

module OboeLang.ScSyntax
  (

    Orchestra(..)
  , Instrument(..)
  , Stmt(..)
  , PrimCall(..)
  , CompValue(..)
  , VarName
  , Value(..)

  , ppOrchestra
  , ppInstr

  ) where

import OboeLang.ScExprPretty
import OboeLang.SyntaxBase hiding ( unaryDocE, binDocE, relDocE )
import OboeLang.Utils.Pretty
import OboeLang.Utils.PrettyExprHPJ
import OboeLang.Utils.Common ( truncatedDouble ) 




-- | At some point may re-introduce global statements...
--
data Orchestra = Orchestra
    { orch_instrs       :: [Instrument]
    }
  deriving (Eq,Show)


data Instrument = Instrument
      { instr_name      :: String
      , instr_body      :: [Stmt]
      }
  deriving (Eq,Show)


type CodeBlock = [Stmt]


-- Note - Sc can have array ops inside synthdefs, particularly 
-- ScBook fig 2.1 is going to be problematic to encode.
-- 
-- Sc not as /monadic/ as Csound
--
data Stmt  = VarDef  VarId (Maybe Value)
           | ArgDef  VarId (Maybe Value)
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
           | UnaryV     UnaryOp     Value
           | BinV       BinOp       Value   Value
           | CondV      CompValue   Value   Value
           | FunCallV   Ident       [Value]
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Pretty print


ppOrchestra :: Orchestra -> Doc
ppOrchestra (Orchestra {orch_instrs = insts})  = 
    vcat $ map (\i -> ppInstr i $+$ blank) insts


ppInstr :: Instrument -> Doc
ppInstr (Instrument name body) = withAlignment TRAIL_SEP $ 
    lparen $+$ (text "SynthDef" <> blockCommaParens1 bodys <> text ".add;") $+$ rparen
  where
    bodys = [ doubleQuotes (text name) , stmts ]
    stmts = blockSemiBraces1 $ map ppStmt body
      

ppStmt :: Stmt -> Doc
ppStmt (VarDef vid opt)          = text "var" <+> liftDoc (ppVarId vid) <> rhs
  where
    rhs = case opt of Nothing -> empty; Just v ->  text " =" <+> ppValue v

ppStmt (ArgDef vid opt)          = text "arg" <+> liftDoc (ppVarId vid) <> rhs
  where
    rhs = case opt of Nothing -> empty; Just v ->  text " =" <+> ppValue v

ppStmt (Assign1 vid p)          = liftDoc (ppVarId vid) <+> ppPrimCall p

ppStmt (Assign2 v1 v2 p)        = lhs <+> ppPrimCall p
  where
    lhs = commaSep $ map (liftDoc . ppVarId) [ v1, v2 ]

ppStmt (Assign3 v1 v2 v3 p)     = lhs <+> ppPrimCall p
  where
    lhs = commaSep $ map (liftDoc . ppVarId) [ v1, v2, v3]

ppStmt (Assign4 v1 v2 v3 v4 p)  = lhs <+> ppPrimCall p
  where
    lhs = commaSep $ map (liftDoc . ppVarId) [v1,v2,v3,v4]

ppStmt (OpCall p)               = ppPrimCall p

ppStmt (Ifte cv b1 b2)          = 
    text "if" <+> parens (ppCompValue cv) 
                   <+> text "then"
                   $+$ nest (vcat $ map ppStmt b1)
                   $+$ text "else"
                   $+$ nest (vcat $ map ppStmt b2)
                   $+$ text "endif"


ppPrimCall :: PrimCall -> Doc
ppPrimCall (PrimCall pid es) = liftDoc (ppIdent pid) <+> tupled (map ppValue es)


ppCompValue :: CompValue -> Doc
ppCompValue (CompValue op v1 v2) = 
    unparse $ relDocE op (exprValue v1) (exprValue v2)


ppValue :: Value -> Doc
ppValue = unparse . exprValue

exprValue :: Value -> DocE
exprValue (Int i)                   = Atom $ int i
exprValue (Float d)                 = 
    Atom $ text $ truncatedDouble $ realToFrac d

exprValue (String s)                = Atom $ doubleQuotes $ text s
exprValue (Var v)                   = Atom $ text v
exprValue (UnaryV op v)             = unaryDocE op (exprValue v)
exprValue (BinV op v1 v2)           = binDocE op (exprValue v1) (exprValue v2)
exprValue (CondV cv v1 v2)          = 
    Atom $ parens (ppCompValue cv <+> char '?'   <+> ppValue v1
                                  <+> char ':'   <+> ppValue v2 )

exprValue (FunCallV pid vs)         = 
    Atom $ liftDoc (ppIdent pid) <> tupled (map ppValue vs)


