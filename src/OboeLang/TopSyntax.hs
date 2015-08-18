{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.TopSyntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Top level syntax.
--
--------------------------------------------------------------------------------

module OboeLang.TopSyntax
  (

  -- * Syntax
    Program(..)
  , Module(..)
  , TopBinding(..)
  , LoadDirective(..)
  , Binding(..)
  , BindingRhs(..)
  , DoBlock
  , Stmt(..)
  , VarBind(..)
  , Decl(..)
  , Expr(..)
  , Literal(..)
  , Ident(..)


  ) where

import OboeLang.SyntaxCommon
import OboeLang.Utils.Pretty
import OboeLang.Utils.PrettyExprH98
import OboeLang.Utils.Common ( truncatedDouble )


import Text.PrettyPrint.HughesPJ                -- package: pretty


data Program = Program
     { prog_main                :: Module
     , prog_extra_libs          :: [Module]
     }
  deriving (Eq,Show)


-- Module is a PrimForm with a special syntax 
-- (not inside a semi-braces block).
--
data Module = Module
    { module_name               :: Ident
    , module_binds              :: [TopBinding]
    }
  deriving (Eq,Show)

data TopBinding = ExplicitBind Ident    BindingRhs
                | ImplicitBind Expr                     -- Just load and loadCore
  deriving (Eq,Show) 

-- Probably want top-level declarations instead of bindings

data LoadDirective = LOAD_CORE | LOAD_USER
  deriving (Enum,Eq,Show)



data Binding = Binding Ident BindingRhs
  deriving (Eq,Show)

data BindingRhs = BindsM [Ident] DoBlock 
                | BindsP [Ident] Expr
  deriving (Eq,Show)


type DoBlock = [Stmt]

-- | In TopSyntax lambdas are /statements/ rather than 
-- expressions.
--
-- This is to keep the separation between do-stmts and pure 
-- expressions.
--
data Stmt = Return    Expr
          | Call      Expr      [Expr]
          | DoBind    VarBind   DoBlock
          | DoLet     Decl
          | DoLam     Ident     [Ident]   DoBlock
          | DoIf      Expr      DoBlock   DoBlock
  deriving (Eq,Show)


data VarBind = Bind1     Ident
             | BindTuple [Ident]
  deriving (Eq,Show)

-- | Decls only support binding simple values (cannot bind 
-- functions).
--
data Decl = Decl Ident Expr
  deriving (Eq,Show)


-- | Expressions are pure (?)
--
-- Applications are builtin Csound (pure) functions like abs.
--
-- There are no pfields in the top level syntax - pfields are
-- positional and hence not modular. We synthesize them from the 
-- orchestra spec later in compilation.
-- 
data Expr = Lit         Literal
          | Var         Ident
          | App         Expr      [Expr]
          | Cond        Expr      Expr      Expr
          | UnaryE      UnaryOp   Expr
          | BinE        BinOp     Expr      Expr
          | RelE        RelOp     Expr      Expr
          | Tuple       [Expr]
          | PrimForm    [Binding]
          | FormLoad    LoadDirective String
          | FormExtend  Expr      Expr
          | FormDeref   Expr      Ident
  deriving (Eq,Show)


data Literal = Bool     Bool
             | Int      Int
             | Float    Decimal
             | String   String
  deriving (Eq,Show)




--------------------------------------------------------------------------------
-- Pretty printing


instance Pretty Program where
  pretty (Program { prog_main         = main_mod
                  , prog_extra_libs   = modules }) = 
    vspaced $ pretty main_mod : map pretty modules


instance Pretty Module where
  pretty (Module { module_name      = name
                 , module_binds     = binds }) = 
    let modu  = text "# module:" <+> pretty name
        bdocs = map pretty binds
    in vspaced $ modu : bdocs


instance Pretty TopBinding where
  pretty (ExplicitBind vid rhs) = pretty vid <+> char '=' <+> pretty rhs <> char ';'
  pretty (ImplicitBind fexpr)   = pretty fexpr <> char ';'


instance Pretty Binding where
  pretty (Binding vid body) 
    | isForm body = pretty vid <+> char '=' 
                      $+$ nest 2 (pretty body)
    | otherwise   = pretty vid <+> char '=' <+> pretty body

isForm :: BindingRhs -> Bool
isForm (BindsP _ (PrimForm {})) = True
isForm _                        = False


instance Pretty BindingRhs where
  pretty (BindsM args body) = methodDefnBody args (pretty body)
  pretty (BindsP args body) = methodDefnBody args (pretty body)


methodDefnBody :: [Ident] -> Doc -> Doc
methodDefnBody []   body = body
methodDefnBody args body = let formals = hsep $ map pretty args
                           in char '\\' <> formals <+> text "->" <+> body


instance Pretty Decl where
  pretty (Decl vid e)             = pretty vid <+> char '=' <+> pretty e


instance Pretty DoBlock where
  pretty [x]                   = pretty x
  pretty xs                    = text "do" <+> blockSemiBraces1 LEAD_SEP (map pretty xs)

instance Pretty Stmt where
  pretty (Return e)             = text "return" <+> pretty e
  pretty (Call e es)            = pretty e <> parens (commaSep (map pretty es))
  pretty (DoBind a de)          = pretty a <+> text "<-" <+> pretty de
  pretty (DoLet d)              = text "let" <+> pretty d

  pretty (DoLam s args de)      = 
    text "letfun" <+> pretty s <+> hsep (map pretty args) 
                  <+> char '=' <+> pretty de 

  pretty (DoIf c d1 d2)         = 
    text "if" <+> pretty c <+> nest 2 (    text "then" <+> pretty d1
                                       $+$ text "else" <+> pretty d2)


instance Pretty VarBind where
  pretty (Bind1 v)             = pretty v
  pretty (BindTuple vs)        = parens (commaSep $ map pretty vs)


instance Pretty Expr where
  pretty = unparse . exprDoc

exprDoc :: Expr -> DocE
exprDoc (Lit v)                 = Atom $ pretty v
exprDoc (Var s)                 = Atom $ pretty s

exprDoc (App vid es)            = 
    Atom $ pretty vid <> parens (commaSep $ map pretty es)

exprDoc (Cond c e1 e2)          = 
    Atom $ text "cond" <+> pretty c <+> char ':' <+> pretty e1
                                    <+> char '?' <+> pretty e2

exprDoc (UnaryE op e)           = unaryDocE op (exprDoc e)
exprDoc (BinE op e1 e2)         = binDocE op (exprDoc e1) (exprDoc e2)
exprDoc (RelE relop e1 e2)      = relDocE relop (exprDoc e1) (exprDoc e2)
exprDoc (Tuple es)              = Atom $ tupled (map pretty es) 

exprDoc (PrimForm bs)           = 
    Atom $ nest 2 (blockSemiBraces1 LEAD_SEP (map pretty bs))

exprDoc (FormLoad d name)       = Atom $ case d of
    LOAD_CORE -> text "loadCore" <> parens (doubleQuotes $ text name)
    LOAD_USER -> text "load"     <> parens (doubleQuotes $ text name)

exprDoc (FormExtend f1 f2)      = genConcatB "@" (exprDoc f1) (exprDoc f2)

exprDoc (FormDeref e s)         = formDerefB (exprDoc e) (getIdent s)

instance Pretty Literal where
  pretty (Bool b)               = text $ show b
  pretty (Int i)                = int i
  pretty (Float d)              = text $ truncatedDouble $ realToFrac d
  pretty (String s)             = doubleQuotes $ text s


