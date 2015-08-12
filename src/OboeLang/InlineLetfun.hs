{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.InlineLetfun
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Inline local function defs. 
--
--------------------------------------------------------------------------------

module OboeLang.InlineLetfun
  ( 
    inlineLetfun

  ) where

import qualified OboeLang.Alpha as Alpha
import OboeLang.CompilerMon
import OboeLang.Subst
import OboeLang.Syntax
import OboeLang.SyntaxCommon


import Control.Applicative
import qualified Data.Map as Map


data FunDefn = FunDefn [VarId] Expr
  deriving (Eq,Show)

type LocalFunDict    = Map.Map VarId FunDefn
type Env             = LocalFunDict


empty_env :: Env
empty_env = Map.empty

findDefn :: Env -> VarId -> Maybe FunDefn
findDefn = flip Map.lookup

extend :: VarId -> FunDefn -> Env -> Env
extend = Map.insert


inlineLetfun :: Program -> Compiler Program
inlineLetfun prgm@(Program { prgm_instrs = instrs }) = 
    do { instrs1 <- mapM inlineInstrument instrs
       ; return $ prgm { prgm_instrs = instrs1 }
       }

-- Don\'t initNameGen
inlineInstrument :: Instrument -> Compiler Instrument
inlineInstrument inst@(Instrument { instr_body = main_expr }) =  
    do { main1 <- inlineExpr empty_env main_expr
       ; return $ inst { instr_body = main1 }
       }


inlineExpr :: Env -> Expr -> Compiler Expr
inlineExpr _   (Return val)                 = return $ Return val

inlineExpr _   (OpCall vid vs)              = return $ OpCall vid vs

inlineExpr env (FunApp vid vs)              = case findDefn env vid of
    Nothing -> throwError "InlineLetfun" "Application of unbound function."
    Just (FunDefn args body) -> do { senv <- insertsArgs args vs Map.empty
                                   ; e1   <- Alpha.alphaK_Expr =<< substArgs senv body
                                   ; inlineExpr env e1
                                   }

inlineExpr _   (FormRef vid)                =  return $ FormRef vid

inlineExpr env (LetValue vid val e)         = 
    LetValue vid val <$> inlineExpr env e

inlineExpr env (LetSig1 vid e1 e2)          = 
    LetSig1 vid <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (LetSig2 v1 v2 e1 e2)        = 
    LetSig2 v1 v2 <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (LetSig3 v1 v2 v3 e1 e2)     = 
    LetSig3 v1 v2 v3 <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (LetSig4 v1 v2 v3 v4 e1 e2)  = 
    LetSig4 v1 v2 v3 v4 <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (LetTuple vs e1 e2)          = 
    LetTuple vs <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (LetFun v args e1 e2)        = 
    do { ea     <- inlineExpr env e1 
       ; let env1 = extend v (FunDefn args ea) env
       ; inlineExpr env1 e2
       }

inlineExpr env (Ifte val e1 e2)             =
    Ifte val <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (e1 :>> e2)                  =
    (:>>) <$> inlineExpr env e1 <*> inlineExpr env e2



