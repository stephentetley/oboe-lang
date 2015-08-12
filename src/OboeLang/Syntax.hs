{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Syntax
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Main intermediate syntax. Two levels - pure values and 
-- effectful expressions.
--
--------------------------------------------------------------------------------

module OboeLang.Syntax
  (
    Program(..)
  , Instrument(..)
  , Form(..)
  , Bindings
  , BindingRhs(..)
  , Expr(..)
  , Value(..)
  
  , effect

  -- * Utils

  , FreeVars
  , freevars

  , PrimIds
  , primIds


  , find

  , mapM_Instrument
  , foreachM_Instrument

  ) where

import OboeLang.SyntaxCommon
import OboeLang.Utils.Common
import OboeLang.Utils.Pretty
import OboeLang.Utils.PrettyExprH98

import Text.PrettyPrint.HughesPJ                -- package: pretty

import qualified Data.Map as Map
import Data.Set ( (\\) )
import qualified Data.Set as Set



data Program = Program
    { prgm_instrs       :: [Instrument]
    , prgm_root_form    :: Form
    }
  deriving (Eq,Show)



-- | 
data Instrument = Instrument
    { instr_num         :: Int
    , instr_body        :: Expr
    }
  deriving (Eq,Show)




data Form = Form Bindings
  deriving (Eq,Show)

type Bindings = Map.Map Ident BindingRhs

data BindingRhs = BindsE [VarId] Expr
                | BindsV Value
                | BindsF Form
  deriving (Eq,Show)



infixl 1 :>>

-- | Prim is prim-application.
--
-- LetFun uses a VarId rather than Ident so the name can be alpha-renamed
--
data Expr = Return      Value                         -- lift a pure Value (needed?)
          | OpCall      Ident     [Value]
          | FormRef     Ident                         -- eliminated by inlining
          | FunApp      VarId     [Value]
          | LetValue    VarId     Value     Expr          -- float, int, bool, ...
          | LetSig1     VarId     Expr      Expr          -- ISig, KSig, ASig, ...
          | LetSig2     VarId     VarId     Expr    Expr
          | LetSig3     VarId     VarId     VarId   Expr    Expr
          | LetSig4     VarId     VarId     VarId   VarId   Expr    Expr
          | LetTuple    [VarId]   Expr      Expr          -- eliminated after expand_main
          | LetFun      VarId     [VarId]   Expr    Expr
          | Ifte        Value     Expr      Expr
          | Expr        :>>       Expr                           -- sequence
  deriving (Eq,Show)

-- TODO (?) - (| LetSigTup [VarId] Expr Expr |) would be more general than 
-- LetSig2, LetSig3, LetSig4...


-- | Separation of Values and Expressions.
--
-- Bool helps const folding even though Csound doesn\'t have one.
--
-- Warning - Pfield is anti-modular and should only be introduced by 
-- init-expressions.
--
data Value = Unit
           | Bool       Bool
           | Int        Int
           | Float      Decimal
           | String     String
           | Var        VarId
           | FormRefV   Ident                   -- to be eliminated
           | Pfield     Int
           | Tuple      [Value]
           | UnaryV     UnaryOp   Value
           | BinV       BinOp     Value     Value
           | RelV       RelOp     Value     Value
           | CondV      Value     Value     Value
           | FunCallV   Ident     [Value]
  deriving (Eq,Show)


-- 
-- Note - Csound if expressions must have a binary relational 
-- expression for the cond expression, the following
-- will not compile:
-- 
-- > if 0 then ...
--
-- The syntax allows this so we must do something in the 
-- translation...
--



-- | Note the type of an Opcode call (Prim) is its answer type
-- (i.e. the application of Prim to its arguments) not the type 
-- of the opcode function.
--
-- C.f. Haskell
--
-- > :t ( 1 + 2)  == Int  (Prim App)
-- 
-- > :t (+)       == Int -> Int -> Int (Prim Def)
--
--



-- | Sequencing is expected to be effectful in the left...
-- 
effect :: Expr -> Bool
effect (Return {})              = False
effect (OpCall {})              = True
effect (FormRef {})             = True          -- Assume True should have been inlined...
effect (FunApp {})              = True
effect (LetValue _ _ e1)        = effect e1
effect (LetSig1 _ e1 e2)        = effect e1 || effect e2
effect (LetSig2 _ _ e1 e2)      = effect e1 || effect e2
effect (LetSig3 _ _ _ e1 e2)    = effect e1 || effect e2
effect (LetSig4 _ _ _ _ e1 e2)  = effect e1 || effect e2
effect (LetTuple _ e1 e2)       = effect e1 || effect e2
effect (LetFun _ _ e1 e2)       = effect e1 || effect e2
effect (Ifte _ e1 e2)           = effect e1 || effect e2
effect (e1 :>> e2)              = effect e1 || effect e2



--------------------------------------------------------------------------------
-- pretty print

instance Pretty Program where
  pretty (Program { prgm_instrs    = instrs
                   , prgm_root_form = root }) = 
    text "orch" $+$ (blockSemiBraces LEAD_SEP $ map ppInstrument instrs)
                $+$ blank
                $+$ pretty root


ppInstrument :: Instrument -> Doc
ppInstrument (Instrument { instr_num        = num
                         , instr_body       = body }) = 
    text "instr" <+> int num <+> char '=' $+$ nest 2 (pretty body)


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
  pretty (BindsE args body) = bindingRhsBody args (pretty body)
  pretty (BindsV val)       = bindingRhsBody [] (pretty val)
  pretty (BindsF body)      = pretty body


bindingRhsBody :: [VarId] -> Doc -> Doc
bindingRhsBody []   body = body
bindingRhsBody args body = let formals = hsep $ map ppVarId args
                           in char '\\' <> formals <+> text "->" <+> body


instance Pretty Expr where
  pretty = unparse . exprDoc



exprDoc :: Expr -> DocE
exprDoc (Return v)                  = valueDoc v
exprDoc (OpCall vid vals)           = 
    Atom $ pretty vid <+> commaSep (map pretty vals)

exprDoc (FormRef v)                 = Atom $ text "REF:" <> pretty v

exprDoc (FunApp v vals)             = 
    Atom $ ppVarId v <+> commaSep (map pretty vals)


exprDoc (LetValue vid v1 e1)        = 
    let pre = text "let" <+> ppVarId vid
    in Atom $ letBody pre (pretty v1) (pretty e1)

exprDoc (LetSig1 vid e1 e2)         = 
    let pre = text "letsig" <+> ppVarId vid
    in Atom $ letBody pre (pretty e1) (pretty e2)

exprDoc (LetSig2 v1 v2 e1 e2)       = 
    let pre = text "letsig2" <+> tupled (map ppVarId [v1,v2])
    in Atom $ letBody pre (pretty e1) (pretty e2)

exprDoc (LetSig3 v1 v2 v3 e1 e2)    = 
    let pre = text "letsig3" <+> tupled (map ppVarId [v1,v2,v3])
    in Atom $ letBody pre (pretty e1) (pretty e2)

exprDoc (LetSig4 v1 v2 v3 v4 e1 e2) = 
    let pre = text "letsig4" <+> tupled (map ppVarId  [v1,v2,v3,v4])
    in Atom $ letBody pre (pretty e1) (pretty e2)

exprDoc (LetTuple vs e1 e2)         = 
    let pre = text "lettuple" <+> tupled (map ppVarId vs)
    in Atom $ letBody pre (pretty e1) (pretty e2)

exprDoc (LetFun v args e1 e2)       = 
    let pre = text "letfun" <+> ppVarId v <+> hsep (map ppVarId args) 
    in Atom $ letBody pre (pretty e1) (pretty e2)

exprDoc (Ifte v1 e1 e2)             = 
    Atom $ text "if" <+> pretty v1
                     <+> nest 2 (    text "then" <+> pretty e1
                                 $+$ text "else" <+> pretty e2)

exprDoc (e1 :>> e2)                 = monadicSeqB (exprDoc e1) (exprDoc e2)  


instance Pretty Value where
  pretty = unparse . valueDoc

valueDoc :: Value -> DocE
valueDoc (Unit)                     = literal "()"
valueDoc (Bool True)                = Atom $ text "true" 
valueDoc (Bool False)               = Atom $ text "false"
valueDoc (Int i)                    = Atom $ int i
valueDoc (Float d)                  = 
    Atom $ text $ truncatedDouble $ realToFrac d

valueDoc (String ss)                = Atom $ doubleQuotes $ text ss
valueDoc (Var v)                    = literal $ varName v 
valueDoc (FormRefV v)               = literal $ getIdent v 
valueDoc (Pfield i)                 = literal $ 'p' : show i
valueDoc (Tuple vs)                 = Atom $ tupled $ map pretty vs 
valueDoc (UnaryV op v)              = unaryDocE op (valueDoc v)
valueDoc (BinV op v1 v2)            = binDocE op (valueDoc v1) (valueDoc v2)
valueDoc (RelV relop v1 v2)         = 
    relDocE relop (valueDoc v1) (valueDoc v2)

valueDoc (CondV vc v1 v2)           = 
    Atom $ text "cond" <+> pretty vc <+> char '?' <+> pretty v1
                                     <+> char ':' <+> pretty v2

valueDoc (FunCallV v es)            = 
    funAppB (literal $ getIdent v) (map valueDoc es)


letBody :: Doc -> Doc -> Doc -> Doc
letBody d e body = 
    d <+> char '=' <+> e <+> text "in" $+$ nest 2 body


--------------------------------------------------------------------------------
-- Free vars

type FreeVars = Set.Set VarId

freevars :: Expr -> FreeVars
freevars (Return v)                     = freevarsV v

freevars (OpCall _ xs)                  = Set.unions $ map freevarsV xs

freevars (FormRef {})                   = Set.empty

freevars (FunApp v xs)                  = 
    Set.insert v $ Set.unions $ map freevarsV xs

freevars (LetValue v val e2)            =
    Set.union (freevarsV val) (Set.delete v $ freevars e2)

freevars (LetSig1 v e1 e2)              =
    Set.union (freevars e1) (Set.delete v $ freevars e2)

freevars (LetSig2 v1 v2 e1 e2)          =
    let lhs = freevars e1
        rhs = Set.delete v2 $ Set.delete v1 $ freevars e2
    in Set.union lhs rhs
                             
freevars (LetSig3 v1 v2 v3 e1 e2)       = 
    let lhs = freevars e1
        rhs = Set.delete v3 $ Set.delete v2 $ Set.delete v1 $ freevars e2
    in Set.union lhs rhs

freevars (LetSig4 v1 v2 v3 v4 e1 e2)    = 
    let lhs = freevars e1
        rhs = Set.delete v4 $ Set.delete v3 $ Set.delete v2 $ Set.delete v1 $ 
                freevars e2
    in Set.union lhs rhs

freevars (LetTuple vs e1 e2)            = 
    let lhs = freevars e1
        rhs = foldr Set.delete (freevars e2) vs
    in Set.union lhs rhs


-- This definition isn\'t really correct 
freevars (LetFun v args e1 e2)          = 
    let zs  = freevars e1 \\ Set.fromList args
        rhs = freevars e2
    in Set.union zs rhs \\ Set.singleton v

freevars (Ifte v1 e1 e2)                = 
    Set.unions [freevarsV v1, freevars e1, freevars e2]

freevars (e1 :>> e2)                    = Set.union (freevars e1) (freevars e2)


freevarsV :: Value -> FreeVars
freevarsV (Unit)                 = Set.empty
freevarsV (Bool {})              = Set.empty
freevarsV (Int {})               = Set.empty
freevarsV (Float {})             = Set.empty
freevarsV (String {})            = Set.empty
freevarsV (Var v)                = Set.singleton v
freevarsV (FormRefV {})          = Set.empty
freevarsV (Pfield {})            = Set.empty
freevarsV (Tuple vs)             = Set.unions $ map freevarsV vs
freevarsV (UnaryV _ v)           = freevarsV v
freevarsV (BinV _ v1 v2)         = freevarsV v1 `Set.union` freevarsV v2
freevarsV (RelV _ v1 v2)         = freevarsV v1 `Set.union` freevarsV v2
freevarsV (CondV v1 vt vf)       = Set.unions $ map freevarsV [v1,vt,vf]
freevarsV (FunCallV _ xs)        = Set.unions $ map freevarsV xs


-- freevarsV _                      = Set.empty


--------------------------------------------------------------------------------

type PrimIds = Set.Set Ident


primIds :: Expr -> PrimIds
primIds (Return v)                  = primIdsV v
primIds (OpCall s xs)               = let s1 = Set.singleton s
                                          ss = map primIdsV xs 
                                      in Set.unions $ s1:ss
primIds (FormRef {})                = Set.empty
primIds (FunApp _ xs)               = Set.unions $ map primIdsV xs
primIds (LetValue _ val e2)         = primIdsV val `Set.union` primIds e2
primIds (LetSig1 _ e1 e2)           = primIds e1 `Set.union` primIds e2
primIds (LetSig2 _ _ e1 e2)         = primIds e1 `Set.union` primIds e2
primIds (LetSig3 _ _ _ e1 e2)       = primIds e1 `Set.union` primIds e2
primIds (LetSig4 _ _ _ _ e1 e2)     = primIds e1 `Set.union` primIds e2
primIds (LetTuple _ e1 e2)          = primIds e1 `Set.union` primIds e2
primIds (LetFun _ _ e1 e2)          = primIds e1 `Set.union` primIds e2
primIds (Ifte v1 e1 e2)             = 
    Set.unions [primIdsV v1, primIds e1, primIds e2]

primIds (e1 :>> e2)                 = Set.union (primIds e1) (primIds e2)


primIdsV :: Value -> PrimIds
primIdsV (Var {})               = Set.empty
primIdsV (Tuple vs)             = Set.unions $ map primIdsV vs
primIdsV (UnaryV _ v)           = primIdsV v
primIdsV (BinV _ v1 v2)         = primIdsV v1 `Set.union` primIdsV v2
primIdsV (RelV _ v1 v2)         = primIdsV v1 `Set.union` primIdsV v2
primIdsV (CondV v1 vt vf)       = Set.unions $ map primIdsV [v1,vt,vf]
primIdsV (FunCallV s xs)        = let s1 = Set.singleton s
                                      ss = map primIdsV xs 
                                  in Set.unions $ s1:ss
primIdsV _                      = Set.empty



find :: Ident -> Form -> Maybe BindingRhs
find name (Form b) = Map.lookup name b

--------------------------------------------------------------------------------
-- Traversals

mapM_Instrument :: (Functor m, Monad m)
                => (Instrument -> m Instrument) 
                -> Program
                -> m Program
mapM_Instrument mf prog@(Program { prgm_instrs = insts }) = 
   (\upd -> prog { prgm_instrs = upd}) <$> mapM mf insts


foreachM_Instrument :: Monad m
                    => (Instrument -> m ()) -> Program -> m ()
foreachM_Instrument mf (Program { prgm_instrs = insts }) = mapM_ mf insts