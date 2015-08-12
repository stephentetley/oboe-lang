{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ToplevelTrans
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translation from top level syntax.
--
--------------------------------------------------------------------------------

module OboeLang.ToplevelTrans
  ( 

    translate

  ) where

import OboeLang.CompilerMon
import OboeLang.FormSyntax
import qualified OboeLang.TopSyntax as T
import OboeLang.SyntaxCommon


import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Text.Printf





-- 
--
translate :: T.Program -> Compiler Program
translate _ = error "translate"

{-
translate (T.Program tmain libs) = 
    do { root <- transModule (T.moduleDict libs) tmain
       ; orch <- transOrchDict =<< getOrchDict
       ; return $ Program { prog_orch = orch
                          , prog_root_form = root
                          }
       }
  where
    getOrchDict = case T.module_mb_orch tmain of
        Just orch -> return orch
        Nothing -> throwError "ToplevelTrans.transProgram"
                              "No orchestra in Main."


transOrchDict :: T.OrchDict -> Compiler OrchDict
transOrchDict dict = T.mapM fn dict
  where
    fn ::  T.BindingRhs -> Compiler BindingRhs
    fn (T.BindsM args body) = BindsM args <$> doBlock body 
    fn (T.BindsP args e)    = BindsP args <$> expression e
    fn (T.BindsF {})        = throwError "ToplevelTrans.transOrchDict"
                                         "Cannot bind to a form expression."


transModule :: T.ModuleDict -> T.Module -> Compiler Form
transModule dict (T.Module { T.module_imports = imports
                           , T.module_binds = binds }) = 
    do { imps   <- mapM (transImport dict) imports
       ; let f1 = extends empty_form imps
       ; f2     <- transBindings binds f1
       ; return f2
       }
       
transImport :: T.ModuleDict -> T.ImportDecl -> Compiler Form
transImport dict (T.ImportTop name) = case T.findModule name dict of
    Nothing -> failMissingModule name
    Just m -> transModule dict m

transImport dict (T.ImportAlias name s) = case T.findModule name dict of
    Nothing -> failMissingModule name
    Just m -> alias <$> transModule dict m
  where
    alias m = extBinding s (BindsF m) empty_form
  

failMissingModule :: ModuleName -> Compiler a
failMissingModule name = 
    throwError "ToplevelTrans.transImport"
               (printf "Missing module %s" (moduleNameString name))


-- Form extension operates on forms that are already bound.
-- (This is very helpful - forward declarations of forms would 
-- be very difficult...)

transBindings :: [T.Binding] -> Form -> Compiler Form
transBindings []     ac = return ac
transBindings (x:xs) ac = transBinding x ac >>= transBindings xs

transBinding :: T.Binding -> Form -> Compiler Form
transBinding (T.Binding name rhs) ac = bindingRhs name rhs ac


bindingRhs :: Ident -> T.BindingRhs -> Form -> Compiler Form
bindingRhs name (T.BindsM args body) ac = 
    do { rhs <- BindsM args <$> doBlock body 
       ; return $ extBinding name rhs ac
       }

bindingRhs name (T.BindsP args e)    ac = 
    do { rhs <- BindsP args <$> expression e
       ; return $ extBinding name rhs ac
       }

bindingRhs name (T.BindsF fe)        ac = 
    do { rhs <- BindsF <$> formExpr fe ac
       ; return $ extBinding name rhs ac
       }


formExpr :: T.FormExpr -> Form -> Compiler Form
formExpr (T.PrimForm binds) _          = F.foldrM transBinding empty_form binds

formExpr (T.FormName vid)    ac        = case find vid ac of
    Just (BindsF fm) -> return fm
    Just _           -> throwError "ToplevelTrans.formExpr"
                                   (printf "Invalid form binding - %s not a form."
                                           (getIdent vid))
    Nothing          -> throwError "ToplevelTrans.formExpr"
                                   (printf "Invalid form binding - %s not bound."
                                           (getIdent vid))

formExpr (T.FormExtension f1 f2) ac    = 
    do { forml <- formExpr f1 ac
       ; formr <- formExpr f2 ac
       ; return $ extend forml formr
       }



-- Code below is just one-to-one mapping.

doBlock :: T.DoBlock -> Compiler DoBlock
doBlock = mapM statement

statement :: T.Stmt -> Compiler Stmt
statement (T.Return e)              = Return <$> expression e
statement (T.PrimCall vid es)       = PrimCall vid <$> mapM expression es
statement (T.DoBind vbind body)     = DoBind <$> varBind vbind <*> doBlock body
statement (T.DoLet decl)            = DoLet <$> declaration decl 
statement (T.DoLam vid args body)   = DoLam vid args <$> doBlock body
statement (T.DoIf ce td fd)         = 
    DoIf <$> expression ce <*> doBlock td <*> doBlock fd


varBind :: T.VarBind -> Compiler VarBind
varBind (T.Bind1 vid)           = pure $ Bind1 vid
varBind (T.BindTuple vs)        = pure $ BindTuple vs


declaration :: T.Decl -> Compiler Decl
declaration (T.Decl vid e)      = Decl vid <$> expression e

expression :: T.Expr -> Compiler Expr
expression (T.Lit v)            = Lit <$> literal v
expression (T.Var vid)          = pure $ Var vid
expression (T.FormRef vid)      = pure $ FormRef vid
expression (T.App vid args)     = App vid <$> mapM expression args

expression (T.Cond ce te fe)    = 
    Cond <$> expression ce <*> expression te <*> expression fe

expression (T.UnaryE uop e)     = UnaryE uop <$> expression e
expression (T.BinE bop e1 e2)   = BinE bop <$> expression e1 <*> expression e2
expression (T.RelE rop e1 e2)   = RelE rop <$> expression e1 <*> expression e2
expression (T.Tuple es)         = Tuple <$> mapM expression es


-- | Literals are the _same_
--
literal :: T.Literal -> Compiler Literal
literal (T.Bool b)              = return $ Bool b
literal (T.Int i)               = return $ Int i
literal (T.Float d)             = return $ Float d
literal (T.String s)            = return $ String s


-}