{-# LANGUAGE PatternGuards              #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ConstFold
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Constant folding for intermediate syntax.
-- 
--------------------------------------------------------------------------------

module OboeLang.ConstFold
  (
    
    constFold

  ) where


import OboeLang.CompilerMon
import OboeLang.Syntax
import OboeLang.SyntaxCommon

import Control.Applicative
import qualified Data.Map as Map


--------------------------------------------------------------------------------
-- Constant folding

type Env = Map.Map VarId Expr


findVal :: VarId -> Env -> Maybe Value
findVal vid env = Map.lookup vid env >>= filt
  where
    filt (Return v) = Just v
    filt _          = Nothing



constFold :: Expr -> Compiler Expr
constFold e = cf Map.empty e


-- | Note - trivial Lets are eliminated with @elimLet@, constant 
-- folding may identify them...
--
cf :: Env -> Expr -> Compiler Expr
cf env (Return (Var x))  
    | Just v <- findVal x env       = return $ Return v

cf _   (Return v)                   = return $ Return v

cf env (OpCall v vs)                = OpCall v <$> mapM (cfV env) vs

cf _   (FormRef v)                  = return $ FormRef v

cf env (FunApp v vs)                = FunApp v <$> mapM (cfV env) vs

cf env (LetValue vid val e1)        = do
    va <- cfV env val 
    let env' = Map.insert vid (Return va) env
    ea <- cf env' e1
    return $ LetValue vid va ea

cf env (LetSig1 vid e1 e2)          = do
    ea <- cf env e1
    let env' = if effect ea then env else Map.insert vid ea env
    eb <- cf env' e2
    return $ LetSig1 vid ea eb

cf env (LetSig2 v1 v2 e1 e2)        = do
    ea <- cf env e1
    eb <- cf env e2
    return $ LetSig2 v1 v2 ea eb

cf env (LetSig3 v1 v2 v3 e1 e2)     = do
    ea <- cf env e1
    eb <- cf env e2
    return $ LetSig3 v1 v2 v3 ea eb

cf env (LetSig4 v1 v2 v3 v4 e1 e2)  = do
    ea <- cf env e1
    eb <- cf env e2
    return $ LetSig4 v1 v2 v3 v4 ea eb

cf env (LetTuple vs e1 e2)          = do
    ea <- cf env e1
    eb <- cf env e2
    return $ LetTuple vs ea eb

cf env (LetFun v vs e1 e2)          = do
    ea <- cf env e1
    eb <- cf env e2
    return $ LetFun v vs ea eb

cf env (Ifte v1 e1 e2)              = 
    go <$> cfV env v1 <*> cf env e1 <*> cf env e2
  where
    go (Bool True)  et _        = et
    go (Bool False) _  ef       = ef
    go cond         et ef       = Ifte cond et ef

cf env (e1 :>> e2)              = (:>>) <$> cf env e1 <*> cf env e2


     
cfV :: Env -> Value -> Compiler Value
cfV env (Var x)                 | Just v <- findVal x env  = pure v
cfV env (UnaryV NEG v1)         = cfNegate  <$> cfV env v1
cfV env (UnaryV op  v1)         = UnaryV op <$> cfV env v1

cfV env (BinV ADD v1 v2)        = cfAdd     <$> cfV env v1 <*> cfV env v2 
cfV env (BinV SUB v1 v2)        = cfSub     <$> cfV env v1 <*> cfV env v2 
cfV env (BinV MUL v1 v2)        = cfMul     <$> cfV env v1 <*> cfV env v2 
cfV env (BinV DIV v1 v2)        = cfDiv     <$> cfV env v1 <*> cfV env v2 
cfV env (BinV MODULUS v1 v2)    = cfMod     <$> cfV env v1 <*> cfV env v2 
cfV env (BinV op v1 v2)         = BinV op   <$> cfV env v1 <*> cfV env v2 

cfV env (RelV EQU v1 v2)        = cfEqu     <$> cfV env v1 <*> cfV env v2 
cfV env (RelV NEQU v1 v2)       = cfNEq     <$> cfV env v1 <*> cfV env v2 
cfV env (RelV LESSTHAN v1 v2)   = cfLT      <$> cfV env v1 <*> cfV env v2 
cfV env (RelV GREATTHAN v1 v2)  = cfGT      <$> cfV env v1 <*> cfV env v2 
cfV env (RelV LTEQU v1 v2)      = cfLTEQ    <$> cfV env v1 <*> cfV env v2 
cfV env (RelV GTEQU v1 v2)      = cfGTEQ    <$> cfV env v1 <*> cfV env v2 

cfV env (CondV vc v1 v2)        = 
    go <$> cfV env vc <*> cfV env v1 <*> cfV env v2
  where
    go (Bool True)  vt _        = vt
    go (Bool False) _  vf       = vf
    go cond         vt vf       = CondV cond vt vf

cfV env (FunCallV v vs)         = FunCallV v <$> mapM (cfV env) vs

cfV _   v                       = pure v


--------------------------------------------------------------------------------
-- Helpers

cfNegate :: Value -> Value
cfNegate (Int i)        = Int (negate i)
cfNegate (Float i)      = Float (negate i)
cfNegate v1             = UnaryV NEG v1


-- | No automatic coercion to floats, even Csound does this...
--
cfAdd :: Value -> Value -> Value 
cfAdd (Int i)   (Int j)    = Int   $ i + j
cfAdd (Float i) (Float j)  = Float $ i + j
cfAdd i         j          = BinV ADD i j

cfSub :: Value -> Value -> Value 
cfSub (Int i)   (Int j)       = Int   $ i - j
cfSub (Float i) (Float j)     = Float $ i - j
cfSub i         j             = BinV SUB i j

cfMul :: Value -> Value -> Value 
cfMul (Int i)   (Int j)       = Int   $ i * j
cfMul (Float i) (Float j)     = Float $ i * j
cfMul i         j             = BinV MUL i j

cfDiv :: Value -> Value -> Value 
cfDiv (Int i)   (Int j)       = Int   $ i `div` j
cfDiv (Float i) (Float j)     = Float $ i / j
cfDiv i         j             = BinV DIV i j

cfMod :: Value -> Value -> Value 
cfMod (Int i)   (Int j)          = Int   $ i `mod` j
cfMod i         j                = BinV MODULUS i j


-- | We don\'t expect conparisons on exotic values so we don\'t
-- optimize them.

cfEqu :: Value -> Value -> Value
cfEqu (Unit)    (Unit)           = Bool True
cfEqu (Bool i)  (Bool j)         = Bool $ i == j
cfEqu (Int i)   (Int j)          = Bool $ i == j
cfEqu (Float i) (Float j)        = Bool $ i == j
cfEqu i         j                = RelV EQU i j


cfNEq :: Value -> Value -> Value
cfNEq (Unit)    (Unit)          = Bool False
cfNEq (Bool i)  (Bool j)        = Bool $ i /= j
cfNEq (Int i)   (Int j)         = Bool $ i /= j
cfNEq (Float i) (Float j)       = Bool $ i /= j
cfNEq i         j               = RelV NEQU i j

cfLT :: Value -> Value -> Value
cfLT (Unit)    (Unit)           = Bool $ () < ()
cfLT (Bool i)  (Bool j)         = Bool $ i < j
cfLT (Int i)   (Int j)          = Bool $ i < j
cfLT (Float i) (Float j)        = Bool $ i < j
cfLT i         j                = RelV LESSTHAN i j

cfGT :: Value -> Value -> Value
cfGT (Unit)    (Unit)           = Bool $ () > ()
cfGT (Bool i)  (Bool j)         = Bool $ i > j
cfGT (Int i)   (Int j)          = Bool $ i > j
cfGT (Float i) (Float j)        = Bool $ i > j
cfGT i         j                = RelV GREATTHAN i j

cfLTEQ :: Value -> Value -> Value
cfLTEQ (Unit)    (Unit)         = Bool $ () <= ()
cfLTEQ (Bool i)  (Bool j)       = Bool $ i <= j
cfLTEQ (Int i)   (Int j)        = Bool $ i <= j
cfLTEQ (Float i) (Float j)      = Bool $ i <= j
cfLTEQ i         j              = RelV LTEQU i j

cfGTEQ :: Value -> Value -> Value
cfGTEQ (Unit)    (Unit)         = Bool $ () >= ()
cfGTEQ (Bool i)  (Bool j)       = Bool $ i >= j
cfGTEQ (Int i)   (Int j)        = Bool $ i >= j
cfGTEQ (Float i) (Float j)      = Bool $ i >= j
cfGTEQ i         j              = RelV GTEQU i j

