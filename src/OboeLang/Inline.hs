{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Inline
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Inline form projections.
--
-- Works on intermediate syntax as we need alpha renaming.
--
--------------------------------------------------------------------------------

module OboeLang.Inline
  ( 
    inline

  ) where

import qualified OboeLang.Alpha as Alpha
import OboeLang.CompilerMon
import OboeLang.Subst
import OboeLang.Syntax
import OboeLang.SyntaxCommon


import Control.Applicative
import qualified Data.Map as Map
import Text.Printf


inline :: Program -> Compiler Program
inline prgm@(Program { prgm_root_form = root }) = 
    mapM_Instrument (inlineInstr root) prgm
  

-- Don\'t initNameGen
inlineInstr :: Form -> Instrument -> Compiler Instrument
inlineInstr env ins@(Instrument { instr_body = body }) =  
    do { main1 <- inlineExpr env body
       ; return $ ins { instr_body = main1 }
       }




inlineExpr :: Form -> Expr -> Compiler Expr
inlineExpr _   (Return val)                 = return $ Return val

inlineExpr _   (OpCall vid vs)              = return $ OpCall vid vs


inlineExpr env (FormRef vid)                = 
    case find vid env of
      Nothing -> throwError "Inline.inlineExpr" 
                            (printf "Cannot find ref %s" (getIdent vid))
      
      -- Syntax is WRONG here and needs a re-think
      Just (BindsE args body) -> let vs = [] in  -- WRONG!!!!
                                 do { senv <- insertsArgs args vs Map.empty
                                    ; e1   <- Alpha.alphaK_Expr =<< substArgs senv body
                                    ; inlineExpr env e1
                                    }


inlineExpr _   (FunApp v vs)                = return $ FunApp v vs

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

inlineExpr env (LetFun v vs e1 e2)          = 
    LetFun v vs <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (Ifte val e1 e2)             =
    Ifte val <$> inlineExpr env e1 <*> inlineExpr env e2

inlineExpr env (e1 :>> e2)                  = 
    (:>>) <$> inlineExpr env e1 <*> inlineExpr env e2

