{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ElimTuple
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Tuple elimination for intermediate repr.
--
--------------------------------------------------------------------------------

module OboeLang.ElimTuple
  (
    elimTuple
  ) where


import OboeLang.CompilerMon
import OboeLang.Syntax
import OboeLang.SyntaxCommon

import Control.Applicative
import Text.Printf

--------------------------------------------------------------------------------
-- Tuple elimination

elimTuple :: Program -> Compiler Program
elimTuple = mapM_Instrument elimInstrument


elimInstrument :: Instrument -> Compiler Instrument
elimInstrument inst@(Instrument { instr_body = main_decl }) =
    do { main1 <- elimExpr main_decl
       ; return $ inst { instr_body = main1 } 
       }



elimExpr :: Expr -> Compiler Expr
elimExpr e = do { initProductiveCount
            ; e1   <- deconsTuple =<< introLetSig e
            ; next <- getProductiveCount
            ; if next > 0 then elimExpr e1 else return e1
            }

introLetSig :: Expr -> Compiler Expr
introLetSig (LetValue v val e)           = LetValue v val <$> introLetSig e

introLetSig (LetSig1 v e1 e2)            =
    LetSig1 v <$> introLetSig e1 <*> introLetSig e2

introLetSig (LetSig2 v1 v2 e1 e2)        =
    LetSig2 v1 v2 <$> introLetSig e1 <*> introLetSig e2

introLetSig (LetSig3 v1 v2 v3 e1 e2)     =
    LetSig3 v1 v2 v3 <$> introLetSig e1 <*> introLetSig e2

introLetSig (LetSig4 v1 v2 v3 v4 e1 e2)  =
    LetSig4 v1 v2 v3 v4 <$> introLetSig e1 <*> introLetSig e2

introLetSig (LetTuple vs e1 e2)          = case e1 of
    (OpCall _ _) -> do { incrProductiveCount
                       ; constr <- makeSigConstr vs
                       ; e      <- introLetSig e2
                       ; return $ constr e1 e
                       }
    _ -> LetTuple vs <$> introLetSig e1 <*> introLetSig e2

introLetSig (Ifte v e1 e2)               =
    Ifte v <$> introLetSig e1 <*> introLetSig e2

introLetSig (e1 :>> e2)                  =
    (:>>) <$> introLetSig e1 <*> introLetSig e2

introLetSig e                            = return e


makeSigConstr :: [VarId] -> Compiler (Expr -> Expr -> Expr)
makeSigConstr [v1,v2]        = return $ LetSig2 v1 v2
makeSigConstr [v1,v2,v3]     = return $ LetSig3 v1 v2 v3
makeSigConstr [v1,v2,v3,v4]  = return $ LetSig4 v1 v2 v3 v4
makeSigConstr xs             = 
    throwError "ElimTuple.makeSigConstr"
               (printf "Bad tuple assignment - arity %d"  (length xs))



deconsTuple :: Expr -> Compiler Expr
deconsTuple (LetValue v val e)           = LetValue v val <$> deconsTuple e
deconsTuple (LetSig1 v e1 e2)            =
    LetSig1 v <$> deconsTuple e1 <*> deconsTuple e2

deconsTuple (LetSig2 v1 v2 e1 e2)        =
    LetSig2 v1 v2 <$> deconsTuple e1 <*> deconsTuple e2

deconsTuple (LetSig3 v1 v2 v3 e1 e2)     =
    LetSig3 v1 v2 v3 <$> deconsTuple e1 <*> deconsTuple e2

deconsTuple (LetSig4 v1 v2 v3 v4 e1 e2)  =
    LetSig4 v1 v2 v3 v4 <$> deconsTuple e1 <*> deconsTuple e2

deconsTuple (LetTuple vs e1 e2)          = do
    deconsTuple e2 >>= \ebody -> pushTuple vs ebody e1

deconsTuple (Ifte v e1 e2)               =
    Ifte v <$> deconsTuple e1 <*> deconsTuple e2

deconsTuple (e1 :>> e2)                  =
    (:>>) <$> deconsTuple e1 <*> deconsTuple e2

deconsTuple e                            = return e

pushTuple :: [VarId] -> Expr -> Expr -> Compiler Expr
pushTuple vs body (Return val)                = case val of
    (Tuple vals) -> incrProductiveCount >> zipLets body vs vals
    _  -> throwError "pushTuple" "Tail of expression not a Tuple"

pushTuple _  _    (OpCall {})                 =
    throwError "ElimTuple.pushTuple" "Tail of expression not a Tuple"

pushTuple _  _    (FormRef {})                =
    throwError "pushTuple" "Tail of expression not a Tuple"

-- Must inline Funs before eliminating tuples
pushTuple _  _    (FunApp {})                 =
    throwError "pushTuple" "Tail of expression FunApp (not expanded)"

pushTuple vs body (LetValue v val e)          =
    LetValue v val <$> pushTuple vs body e

pushTuple vs body (LetSig1 v e1 e2)           =
    LetSig1 v e1 <$> pushTuple vs body e2

pushTuple vs body (LetSig2 v1 v2 e1 e2)       =
    LetSig2 v1 v2 e1 <$> pushTuple vs body e2

pushTuple vs body (LetSig3 v1 v2 v3 e1 e2)    =
    LetSig3 v1 v2 v3 e1 <$> pushTuple vs body e2

pushTuple vs body (LetSig4 v1 v2 v3 v4 e1 e2) =
    LetSig4 v1 v2 v3 v4 e1 <$> pushTuple vs body e2

pushTuple vs body (e1 :>> e2)                 =
    (\ebody -> e1 :>> ebody) <$> pushTuple vs body e2

pushTuple _  _    (LetTuple {})               =
    throwError "pushTuple" "Nesting, unhandled..."

pushTuple _  _    (LetFun {})                 = 
    throwError "pushTuple" "LetFun unhandled"

pushTuple _  _    (Ifte {})                   =
    throwError "pushTuple" "Ifte unhandled"




zipLets :: Expr -> [VarId] -> [Value] -> Compiler Expr
zipLets body = step
  where
     step (x:xs) (y:ys) = LetValue x y <$> step xs ys
     step []     []     = return body
     step _      _      = throwError "ElimTuple.zipLets" "deconsTuple (arity mismatch)"


