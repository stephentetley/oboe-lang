{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Subst
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Substs VarIds for Values
--
--------------------------------------------------------------------------------

module OboeLang.Subst
  ( 
    SubstEnv
  , subst_empty  
  , substArgs
  , insertsArgs
  
  ) where

import OboeLang.CompilerMon
import OboeLang.Syntax
import OboeLang.SyntaxCommon
import OboeLang.Utils.Common
import OboeLang.Utils.Pretty

import Control.Applicative
import qualified Data.Map as Map
import Text.Printf


type SubstEnv = Map.Map VarId Value

subst_empty :: SubstEnv
subst_empty = Map.empty

insertsArgs :: [VarId] -> [Value] -> SubstEnv -> Compiler SubstEnv
insertsArgs []     []       env = return env
insertsArgs (x:xs) (y:ys)   env = insertsArgs xs ys (Map.insert x y env)
insertsArgs xs      ys      _   = 
    throwError "Subst.insertArgs" 
               (printf "Arity mismatch\nVars: %s\nVals: %s" 
                       (prettyVars xs) 
                       (prettyVals ys))

findSubst :: SubstEnv -> VarId -> Maybe Value
findSubst = flip Map.lookup


substArgs :: SubstEnv -> Expr -> Compiler Expr
substArgs env (Return val)                = Return <$> substArgsV env val

substArgs _   (FormRef v)                 = pure $ FormRef v

substArgs env (FunApp vid vals)           = 
    FunApp vid <$> mapM (substArgsV env) vals

substArgs env (OpCall pid vals)           = 
    OpCall pid <$> mapM (substArgsV env) vals

substArgs env (LetValue v1 val e)         =
    LetValue v1 <$> substArgsV env val <*> substArgs env e

substArgs env (LetSig1 v1 e1 e2)          =
    LetSig1 v1 <$> substArgs env e1 <*> substArgs env e2

substArgs env (LetSig2 v1 v2 e1 e2)       =
    LetSig2 v1 v2 <$> substArgs env e1 <*> substArgs env e2

substArgs env (LetSig3 v1 v2 v3 e1 e2)    =
    LetSig3 v1 v2 v3 <$> substArgs env e1 <*> substArgs env e2

substArgs env (LetSig4 v1 v2 v3 v4 e1 e2) =
    LetSig4 v1 v2 v3 v4 <$> substArgs env e1 <*> substArgs env e2

substArgs env (LetTuple vs e1 e2)         =
    LetTuple vs <$> substArgs env e1 <*> substArgs env e2

substArgs env (LetFun v vs e1 e2)         = 
    LetFun v vs <$> substArgs env e1 <*> substArgs env e2

substArgs env (Ifte val et ef)            =
    Ifte <$> substArgsV env val <*> substArgs env et <*> substArgs env ef

substArgs env (e1 :>> e2)                 = 
    (:>>) <$> substArgs env e1 <*> substArgs env e2



-- Values

substArgsV :: SubstEnv -> Value -> Compiler Value
substArgsV env (Var vid)            = case findSubst env vid of
    Nothing -> return $ Var vid
    Just val -> return val

substArgsV env (Tuple vs)           = Tuple <$> mapM (substArgsV env) vs

substArgsV env (UnaryV op v)        = UnaryV op <$> substArgsV env v

substArgsV env (BinV op v1 v2)      = 
    BinV op <$> substArgsV env v1 <*> substArgsV env v2 

substArgsV env (RelV op v1 v2)      =
    RelV op <$> substArgsV env v1 <*> substArgsV env v2 

substArgsV env (CondV vc vt vf)     =
    CondV <$> substArgsV env vc <*> substArgsV env vt <*> substArgsV env vf

substArgsV env (FunCallV pid vs)    = FunCallV pid <$> mapM (substArgsV env) vs

substArgsV _   v                    = return v




prettyVars :: [VarId] -> String
prettyVars = show . commaBrackets . map ppVarId

prettyVals :: [Value] -> String
prettyVals = show . commaBrackets . map pretty






