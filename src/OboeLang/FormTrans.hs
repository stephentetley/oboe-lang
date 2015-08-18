{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OchreLang.FormTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translation from Form syntax.
--
--------------------------------------------------------------------------------

module OboeLang.FormTrans
  ( 

    translate

  ) where

import OboeLang.CompilerMon
import OboeLang.Syntax hiding ( find )
import qualified OboeLang.FormSyntax as F
import OboeLang.SyntaxCommon


import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Text.Printf



type NameEnv = Map.Map Ident VarId

find :: Ident -> NameEnv -> Maybe VarId
find = Map.lookup

extend :: Ident -> VarId -> NameEnv -> NameEnv
extend = Map.insert

extends :: [Ident] -> [VarId] -> NameEnv -> NameEnv
extends xs ys env = foldr (uncurry Map.insert) env (zip xs ys)

initial_env :: NameEnv
initial_env = Map.empty


translate :: F.Program -> Compiler Program
translate (F.Program { F.prog_root_form = root }) =
    Program <$> transOrch root <*> transForm root


transOrch :: F.Form -> Compiler [Instrument]
transOrch _ = error "transOrch - failure"

{-
transOrch :: F.OrchDict -> Compiler [Instrument]
transOrch = mapM (uncurry fn) . IntMap.toAscList 
  where
    fn ix rhs = Instrument ix <$> instrRhs rhs 
-}


transForm :: F.Form -> Compiler Form
transForm (F.Form binds) = Form <$> T.mapM transBindingRhs binds




instrRhs :: F.BindingRhs -> Compiler Expr
instrRhs rhs = transBindingRhs rhs >>= step
  where
    step (BindsE [] expr) = return expr
    step (BindsE _  _)    = throwError "FormTrans.instrRhs"
                                       "Trying to bind to an expression with arguments"

    step (BindsV {})      = throwError "FormTrans.instrRhs"
                                       "Trying to bind to a pure expression"

    step (BindsF {})      = throwError "FormTrans.instrRhs"
                                       "Trying to bind to a Form"


transBindingRhs :: F.BindingRhs -> Compiler BindingRhs
transBindingRhs (F.BindsM args0 blk)    = 
    do { (args,env) <- instantiateArgs initial_env args0
       ; e1         <- transDoBlock env blk
       ; return $ BindsE args e1
       }

transBindingRhs (F.BindsP [] e)         = 
    do { v1         <- exprValue initial_env e
       ; return $ BindsV v1
       }

transBindingRhs (F.BindsP _ _)          = 
    throwError "FormTrans.transBindingRhs"
               "Cannot translate a pure function binding."


instantiateArgs :: NameEnv -> [Ident] -> Compiler ([VarId], NameEnv)
instantiateArgs env []     = return $ ([],env)
instantiateArgs env (x:xs) = 
    do { vid        <- makeVarId x
       ; (vs,env1)  <- instantiateArgs (extend x vid env) xs
       ; return (vid:vs,env1)
       }


transDoBlock :: NameEnv -> F.DoBlock -> Compiler Expr
transDoBlock _   []     = throwError "ToplevelTrans.transDoBlock" "Empty 'do' block"
transDoBlock env (z:zs) = step z zs
  where
    step (F.Return e)               []      = Return <$> exprValue env e
    
    -- Ignore return if not in final position
    step (F.Return _)               (x:xs)  = step x xs

{-
    -- Top level prim call can either be super-call, opcode call or letfun call    
    step (F.PrimCall vid es)        []      = 
        constrXCall env vid <$> mapM (exprValue env) es

    step (F.PrimCall vid es)        xs      =
        do { vs <- mapM (exprValue env) es
           ; e2 <- transDoBlock env xs
           ; let fn = constrXCall env vid
           ; return $ fn vs :>> e2
           }
-}
    -- Bind cannot be last statement in a do-block
    step (F.DoBind _ _)             []      =
        throwError "ToplevelTrans.transDoBlock"
                   "Last statement in a 'do' block must be an expression, saw DoDind."

    step (F.DoBind patt blk)        xs      =
        do { vs         <- varBinds patt
           ; e1         <- transDoBlock env blk
           ; let env1   = extends (bindingVars patt) vs env
           ; e2         <- transDoBlock env1 xs
           ; case vs of
               [] -> return (e1 :>> e2)
               [v1] -> return $ LetSig1 v1 e1 e2
               _   -> return $ LetTuple vs e1 e2
           }

    -- Let cannot be last statement in a do-block
    step (F.DoLet {})                []     =
        throwError "ToplevelTrans.transDoBlock" 
                   "Last statement in a 'do' block must be an expression, saw DoLet."

    step (F.DoLet dc@(F.Decl v0 _)) xs      = 
        do { (vid,val)  <- transDecl env dc
           ; let env1   = extend v0 vid env
           ; expr       <- transDoBlock env1 xs
           ; return $ LetValue vid val expr
           }


    -- Lam (letfun) cannot be last statement in a do-block
    step (F.DoLam {})               []      =
        throwError "ToplevelTrans.transDoBlock" 
                   "Last statement in a 'do' block must be an expression, saw LetFun."
           
    step (F.DoLam s args d1)        xs      = 
        do { vid        <- freshLamId 
           ; vargs      <- mapM makeVarId args
           ; let env1   = extend s vid env
           ; let env2   = extends args vargs env1
           ; ea         <- transDoBlock env2 d1
           ; body       <- transDoBlock env1 xs
           ; return $ LetFun vid vargs ea body
           }

    step (F.DoIf c d1 d2)           xs      =
        do { val   <- exprValue env c
           ; e1    <- transDoBlock env d1
           ; e2    <- transDoBlock env d2
           ; let ans1 = Ifte val e1 e2
           ; if null xs then return ans1 
                        else do { rest <- transDoBlock env xs 
                                ; return $ ans1 :>> rest }
           }                                    


constrXCall :: NameEnv -> Ident -> ([Value] -> Expr)
constrXCall env ident@(Ident name) = case find ident env of
    Just lam -> FunApp lam 
    Nothing -> OpCall (Ident name)



bindingVars :: F.VarBind -> [Ident]
bindingVars (F.Bind1 v)      = [v]
bindingVars (F.BindTuple vs) = vs

varBinds :: F.VarBind -> Compiler [VarId]
varBinds = mapM makeVarId . bindingVars




transDecl :: NameEnv -> F.Decl -> Compiler (VarId,Value)
transDecl env (F.Decl vid e) = (,) <$> makeVarId vid <*> exprValue env e



exprValue :: NameEnv -> F.Expr -> Compiler Value
exprValue _   (F.Lit l)           = literalValue l


-- | A var at this position must be bound.
-- It cannot be a Csound function- or opcode name, nor an Ochre
-- method.
--
exprValue env (F.Var vid)         = case find vid env of
    Nothing -> case makeFinal (getIdent vid) of
                   Just fin -> return $ Var fin
                   Nothing -> throwError "ToplevelTrans.exprValue"  
                                          (printf "Unbound, unreconized variable: %s"
                                                  (getIdent vid))
    Just v -> return $ Var v

exprValue _   (F.FormRef v)       = pure $ FormRefV v

{-
-- | This must be a Csound function (e.g. abs).
-- It cannot be a Csound opcode or a (pre-inlining) Oboe method
-- (Oboe methods are not pure).
--
exprValue env (F.App vid es)      =
    FunCallV vid <$> mapM (exprValue env) es
-}

exprValue env (F.Cond ce e1 e2)   =
    CondV <$> exprValue env ce <*> exprValue env e1 <*> exprValue env e2

exprValue env (F.UnaryE op e)     = UnaryV op <$> exprValue env e

exprValue env (F.BinE op e1 e2)   =
    BinV op <$> exprValue env e1 <*> exprValue env e2

exprValue env (F.RelE op e1 e2)   =
    RelV op <$> exprValue env e1 <*> exprValue env e2

exprValue env (F.Tuple es)        = Tuple <$> mapM (exprValue env) es



literalValue :: F.Literal -> Compiler Value
literalValue (F.Bool b)         = return $ Bool b
literalValue (F.Int i)          = return $ Int i
literalValue (F.Float d)        = return $ Float d
literalValue (F.String s)       = return $ String s



makeVarId :: Ident -> Compiler VarId
makeVarId vid = case identRate vid of
    Just rate -> fresh rate
    Nothing -> throwError "ToplevelTrans.makeVarId"
                          (printf  "Unrecognized rate prefix. Cannot create a VarId for %s "
                                   (getIdent vid))



identRate :: Ident -> Maybe RateId
identRate = fn . getIdent
  where
    fn ('a':_) = Just $ A_RATE
    fn ('k':_) = Just $ K_RATE
    fn ('i':_) = Just $ I_RATE
    fn _       = Nothing


