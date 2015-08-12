{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.FormSyntax
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Syntax with forms /normalized/.
--
--------------------------------------------------------------------------------

module OboeLang.FormSyntax
  (
   
    Program(..)
  , OrchDict
  , Form(..)
  , Bindings
  , BindingRhs(..)
  , DoBlock
  , Stmt(..)
  , VarBind(..)
  , Decl(..)
  , Expr(..)
  , Literal(..)


  -- * Operations
  , empty_form
  , extBinding
  , extend
  , extends
  , find
  , recfind

  ) where


import OboeLang.SyntaxCommon
import OboeLang.Utils.Common
import OboeLang.Utils.Pretty
import OboeLang.Utils.PrettyExprH98


import Text.PrettyPrint.HughesPJ                -- package: pretty

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

-- orch ?
data Program = Program
     { prog_orch                :: OrchDict
     , prog_root_form           :: Form
     }
  deriving (Eq,Show)


type OrchDict = IntMap.IntMap BindingRhs



data Form = Form Bindings
  deriving (Eq,Show)

type Bindings = Map.Map Ident BindingRhs

data BindingRhs = BindsM [Ident] DoBlock 
                | BindsP [Ident] Expr
                | BindsF Form
  deriving (Eq,Show)


type DoBlock = [Stmt]

-- | In TopSyntax lambdas are /statements/ rather than 
-- expressions.
--
-- This is to keep the separation between do-stmts and pure 
-- expressions.
--
data Stmt = Return    Expr
          | PrimCall  Ident     [Expr]
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
data Expr = Lit     Literal
          | Var     Ident
          | FormRef Ident             -- FromRef?
          | App     Ident     [Expr]
          | Cond    Expr      Expr      Expr
          | UnaryE  UnaryOp   Expr
          | BinE    BinOp     Expr      Expr
          | RelE    RelOp     Expr      Expr
          | Tuple   [Expr]
  deriving (Eq,Show)




data Literal = Bool     Bool
             | Int      Int
             | Float    Decimal
             | String   String
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Pretty printing

instance Pretty Program where
  pretty (Program { prog_orch      = orch
                  , prog_root_form = root }) = 
    ppOrchDict orch $+$ namedForm (Ident "Root") root

ppOrchDict :: OrchDict -> Doc
ppOrchDict dict = 
    text "orch" <+> char '=' 
      $+$ blockSemiBraces LEAD_SEP (map (uncurry fn) $ IntMap.toAscList dict)
  where
   fn ix rhs = int ix <+> char '=' <+> pretty rhs



namedForm :: Ident -> Form -> Doc
namedForm vid fm = 
    pretty vid <+> char '=' $+$ nest 2 (pretty fm)

instance Pretty Form where
  pretty (Form binds) = blockSemiBraces LEAD_SEP $ map ppBinding $ Map.toAscList binds

ppBinding :: (Ident,BindingRhs) -> Doc
ppBinding (vid,body) 
    | isForm body = text "def" <+> pretty vid 
                               <+> char '=' <+> pretty body
    | otherwise   = pretty vid <+> char '=' <+> pretty body



isForm :: BindingRhs -> Bool
isForm (BindsF {}) = True
isForm _           = False


instance Pretty BindingRhs where
  pretty (BindsM args body) = bindingRhsBody args (ppDoBlock body)
  pretty (BindsP args body) = bindingRhsBody args (pretty body)
  pretty (BindsF body)      = pretty body


bindingRhsBody :: [Ident] -> Doc -> Doc
bindingRhsBody []   body = body
bindingRhsBody args body = let formals = hsep $ map pretty args
                           in char '\\' <> formals <+> text "->" <+> body


instance Pretty Decl where
  pretty (Decl vid e)             = pretty vid <+> char '=' <+> pretty e


ppDoBlock :: DoBlock -> Doc
ppDoBlock [x]                   = pretty x
ppDoBlock xs                    = text "do" <+> blockSemiBraces1 LEAD_SEP (map pretty xs)

instance Pretty Stmt where
  pretty (Return e)               = text "return" <+> pretty e
  pretty (PrimCall v es)          = pretty v <+> commaSep (map pretty es)
  pretty (DoBind a de)            = ppVarBind a <+> text "<-" <+> ppDoBlock de
  pretty (DoLet d)                = text "let" <+> pretty d
  pretty (DoLam s args de)        = 
    text "letfun" <+> pretty s <+> hsep (map pretty args) 
                  <+> char '=' <+> ppDoBlock de 
  pretty (DoIf c d1 d2)           = 
    text "if" <+> pretty c <+> nest 2 (    text "then" <+> ppDoBlock d1
                                       $+$ text "else" <+> ppDoBlock d2)


ppVarBind :: VarBind -> Doc
ppVarBind (Bind1 v)             = pretty v
ppVarBind (BindTuple vs)        = parens (commaSep $ map pretty vs)


instance Pretty Expr where
  pretty = unparse . exprDoc

exprDoc :: Expr -> DocE
exprDoc (Lit v)                 = Atom $ pretty v
exprDoc (Var s)                 = Atom $ pretty s
exprDoc (FormRef s)             = Atom $ pretty s
exprDoc (App s es)              = funAppB (exprDoc (Var s)) (map exprDoc es)
exprDoc (Cond c e1 e2)          = 
    Atom $ text "cond" <+> pretty c <+> char ':' <+> pretty e1
                                    <+> char '?' <+> pretty e2

exprDoc (UnaryE op e)           = unaryDocE op (exprDoc e)
exprDoc (BinE op e1 e2)         = binDocE op (exprDoc e1) (exprDoc e2)
exprDoc (RelE relop e1 e2)      = relDocE relop (exprDoc e1) (exprDoc e2)
exprDoc (Tuple es)              = Atom $ tupled (map pretty es) 


instance Pretty Literal where
  pretty (Bool b)              = text $ show b
  pretty (Int i)               = int i
  pretty (Float d)             = text $ truncatedDouble $ realToFrac d
  pretty (String s)            = doubleQuotes $ text s





--------------------------------------------------------------------------------
-- Operations

empty_form :: Form
empty_form = Form $ Map.empty

extBinding :: Ident -> BindingRhs -> Form -> Form
extBinding name rhs (Form b) = Form $ Map.insert name rhs b

-- | Bindings in the right overwrite bindings in the left.
--
extend :: Form -> Form -> Form
extend fa (Form b) = foldr (uncurry extBinding) fa $ Map.toAscList b

-- | Bindings in the right overwrite bindings in the left.
--
extends :: Form -> [Form] -> Form
extends fa []     = fa
extends fa (x:xs) = extends (extend fa x) xs


find :: Ident -> Form -> Maybe BindingRhs
find name (Form b) = Map.lookup name b

recfind :: [Ident] -> Form -> Maybe BindingRhs
recfind xs (Form z) = work xs z
  where
    work []     _   = Nothing
    work [y]    b   = Map.lookup y b
    work (y:ys) b   = case Map.lookup y b of
                         Just (BindsF (Form c)) -> work ys c
                         _ -> Nothing
