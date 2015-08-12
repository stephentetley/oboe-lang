{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Alpha
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Alpha renaming for intermediate syntax.
--
--------------------------------------------------------------------------------

module OboeLang.Alpha
  (
    Env
  , empty_env
  , find
  , findVal
  , insert
  , inserts  
  
  , alphaK_BindingRhs
  
  , alphaExpr
  , alphaK_Expr

  ) where


import OboeLang.CompilerMon
import OboeLang.Syntax hiding ( find ) 
import OboeLang.SyntaxCommon

import Control.Applicative hiding ( empty )
import qualified Data.Map as Map


--------------------------------------------------------------------------------
-- alpha renaming

type Env = Map.Map VarId VarId


empty_env :: Env
empty_env = Map.empty


-- | We need the permissiveness of returning original VarId to 
-- handle variables defined with vardef that wont be renamed
--
find :: Env -> VarId -> Compiler VarId
find env x = maybe (return x) return $ Map.lookup x env


findVal :: Env -> Value -> Compiler Value
findVal env (Var v)     = Var <$> find env v
findVal _   v           = pure v


findArgs :: Env -> [VarId] -> Compiler [VarId]
findArgs env vs = mapM (find env) vs


insert :: (VarId,VarId) -> Env -> Env
insert (a,b) env = Map.insert a b env

inserts :: [(VarId,VarId)] -> Env -> Env
inserts vvs env = foldr insert env vvs

alphaK_BindingRhs :: BindingRhs -> Compiler BindingRhs
alphaK_BindingRhs (BindsE args e) = do
    pairs <- mapM (\a -> do { a1 <- refresh a; return (a,a1)}) args
    let env = inserts pairs Map.empty
    e1 <- ac env e
    return $ BindsE (map snd pairs) e1

alphaK_BindingRhs (BindsV v)      = BindsV <$> acV empty_env v

alphaK_BindingRhs (BindsF _a)     = error "alphaK_BindingsRhs"
    

-- | Re-initialize counts to one...
--
alphaExpr :: Expr -> Compiler Expr
alphaExpr e = initNameGen >> ac Map.empty e

-- | Don\'t initialize variable counts
--
alphaK_Expr :: Expr -> Compiler Expr
alphaK_Expr e = ac Map.empty e


ac :: Env -> Expr -> Compiler Expr
ac env (Return val)                 = Return <$> acV env val

-- Don\'t alpha rename opcodes.
ac env (OpCall pid vs)              = OpCall pid <$> mapM (acV env) vs

-- Don\'t alpha rename form refs.
ac _   (FormRef v)                  = pure $ FormRef v

ac env (FunApp v vs)                = 
    FunApp <$> find env v <*> mapM (acV env) vs

ac env (LetValue vid val e1)        = do
    vx <- refresh vid
    va <- acV env val
    ea <- ac (insert (vid,vx) env) e1
    return $ LetValue vx va ea

ac env (LetSig1 vid e1 e2)          = do
    vx <- refresh vid
    ea <- ac env e1
    eb <- ac (insert (vid,vx) env) e2
    return $ LetSig1 vx ea eb

ac env (LetSig2 v1 v2 e1 e2)        = do
    xa <- refresh v1
    xb <- refresh v2
    ea <- ac env e1
    eb <- ac (inserts [(v1,xa), (v2,xb)] env) e2
    return $ LetSig2 xa xb ea eb

ac env (LetSig3 v1 v2 v3 e1 e2)     = do
    xa <- refresh v1
    xb <- refresh v2
    xc <- refresh v3
    ea <- ac env e1
    eb <- ac (inserts [(v1,xa), (v2,xb), (v3,xc)] env) e2
    return $ LetSig3 xa xb xc ea eb

ac env (LetSig4 v1 v2 v3 v4 e1 e2)  = do
    xa <- refresh v1
    xb <- refresh v2
    xc <- refresh v3
    xd <- refresh v4
    ea <- ac env e1
    eb <- ac (inserts [(v1,xa), (v2,xb), (v3,xc), (v4,xd)] env) e2
    return $ LetSig4 xa xb xc xd ea eb

ac env (LetTuple vs e1 e2)          = do
    xs <- mapM refresh  vs
    ea <- ac env e1
    eb <- ac (inserts (zip vs xs) env) e2
    return $ LetTuple xs ea eb

ac env (LetFun v vs e1 e2)        = do 
    v1         <- freshLamId
    let env1   = insert (v,v1) env
    pairs      <- mapM (\x -> do { x1 <- refresh x; return (x,x1) }) vs
    let env2   = inserts pairs env1
    name       <- find env2 v
    args       <- findArgs env2 vs
    ea         <- ac env2 e1
    body       <- ac env1 e2    -- Body is in env1
    return $ LetFun name args ea body

ac env (Ifte v1 e1 e2)              = 
    Ifte <$> findVal env v1 <*> ac env e1 <*> ac env e2

ac env (e1 :>> e2)                  = 
    (:>>) <$> ac env e1 <*> ac env e2



acV :: Env -> Value -> Compiler Value
acV env (Var vid)               = Var <$> find env vid
acV env (Tuple vs)              = Tuple <$> mapM (acV env) vs
acV env (FunCallV v vs)         = FunCallV v <$> mapM (acV env) vs
acV env (UnaryV op v)           = UnaryV op <$> acV env v
acV env (BinV op v1 v2)         = BinV op <$> acV env v1 <*> acV env v2
acV env (RelV op v1 v2)         = RelV op <$> acV env v1 <*> acV env v2
acV env (CondV v1 vt vf)        = 
    CondV <$> acV env v1 <*> acV env vt <*> acV env vf

acV _   val                     = return val


