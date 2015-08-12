{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Beta
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Beta reduction.
--
--------------------------------------------------------------------------------

module OboeLang.Beta
  (
    
    beta

  ) where

import OboeLang.Alpha ( Env, find, findVal, insert )
import OboeLang.CompilerMon
import OboeLang.Syntax hiding ( find )

import Control.Applicative
import qualified Data.Map as Map


--------------------------------------------------------------------------------
-- beta reduction

-- Env : Var -> Var


beta :: Expr -> Compiler Expr
beta = red Map.empty


red :: Env -> Expr -> Compiler Expr
red env (Return val)                = Return <$> redV env val

red env (OpCall v vs)               = OpCall v <$> mapM (redV env) vs

red _   (FormRef v)                 = pure $ FormRef v

red env (FunApp v vs)               = FunApp v <$> mapM (redV env) vs

red env (LetValue vid val e1)       = redV env val >>= \ans -> case ans of
    (Var v)    -> red (insert (vid,v) env) e1
    _          -> LetValue vid val <$> red env e1

red env (LetSig1 vid e1 e2)         = red env e1 >>= \ans -> case ans of
    Return (Var v)    -> red (insert (vid,v) env) e2
    ea                -> LetSig1 vid ea <$> red env e2

red env (LetSig2 v1 v2 e1 e2)       = 
    LetSig2 v1 v2 <$> red env e1 <*> red env e2

red env (LetSig3 v1 v2 v3 e1 e2)    = 
    LetSig3 v1 v2 v3 <$> red env e1 <*> red env e2

red env (LetSig4 v1 v2 v3 v4 e1 e2) = 
    LetSig4 v1 v2 v3 v4 <$> red env e1 <*> red env e2

red env (Ifte v1 e1 e2)             = 
    Ifte <$> findVal env v1 <*> red env e1 <*> red env e2

-- Should have been eliminated by now
red env (LetTuple vs e1 e2)         = 
    LetTuple vs <$> red env e1 <*> red env e2

-- Should have been eliminated by now
red env (LetFun v vs e1 e2)         = 
    LetFun v vs <$> red env e1 <*> red env e2

red env (e1 :>> e2)                 = 
    (:>>) <$> red env e1 <*> red env e2


redV :: Env -> Value -> Compiler Value
redV env (Var vid)              = Var <$> find env vid
redV env (Tuple vs)             = Tuple <$> mapM (redV env) vs
redV env (UnaryV op v)          = UnaryV op <$> redV env v
redV env (BinV op v1 v2)        = BinV op <$> redV env v1 <*> redV env v2
redV env (RelV op v1 v2)        = RelV op <$> redV env v1 <*> redV env v2
redV env (CondV v1 vt vf)       = 
    CondV <$> redV env v1 <*> redV env vt <*> redV env vf

redV env (FunCallV v vs)        = FunCallV v <$> mapM (redV env) vs
redV _   val                    = return val
