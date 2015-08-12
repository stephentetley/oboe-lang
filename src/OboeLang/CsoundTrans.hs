{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.CsoundTrans
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translation to Csound syntax.
--
--------------------------------------------------------------------------------

module OboeLang.CsoundTrans
  (

    translate
  , instr

  ) where

import OboeLang.CompilerMon
import OboeLang.CsoundSyntax
import qualified OboeLang.Syntax as S
import OboeLang.SyntaxCommon
import OboeLang.Utils.Common


import Control.Applicative
import Text.Printf

--------------------------------------------------------------------------------
-- build orch


type Code = H Stmt
     

translate :: S.Program -> Compiler Program
translate (S.Program {S.prgm_instrs = insts}) = 
    Program <$> mapM instr insts


instr :: S.Instrument -> Compiler Instrument
instr (S.Instrument {S.instr_num = i, S.instr_body = body}) =
    (Instrument i . toListH) <$> top body
 


-- Note - all super-calls should have been inlined before final
-- translation.


-- | Translate the top level expression
--
top :: S.Expr -> Compiler Code
top (S.Return {})                   = topError "value"
top (S.OpCall p vs)                 = (wrapH . OpCall) <$> primCall p vs
top (S.FormRef {})                  = topError "form-ref"
top (S.FunApp {})                   = topError "fun-app"
top (S.LetValue vid val e)          = appendH <$> valDecl vid val <*> top e
top (S.LetSig1 vid e1 e2)           = appendH <$> decl1 vid e1 <*> top e2
top (S.LetSig2 v1 v2 e1 e2)         = appendH <$> decl2 v1 v2 e1 <*> top e2
top (S.LetSig3 v1 v2 v3 e1 e2)      = appendH <$> decl3 v1 v2 v3 e1 <*> top e2
top (S.LetSig4 v1 v2 v3 v4 e1 e2)   = appendH <$> decl4 v1 v2 v3 v4 e1 <*> top e2
top (S.LetTuple {})                 = topError "let-tuple (not eliminated)"
top (S.LetFun {})                   = topError "letfun (not eliminated)"
top (S.Ifte cv e1 e2)               =
    (\cond c1 c2 -> wrapH $ Ifte cond (toListH c1) (toListH c2))
      <$> compValue cv <*> top e1 <*> top e2

top (e1 S.:>> e2)                   = appendH <$> effect e1 <*> top e2

topError :: String -> Compiler a
topError cstr =
    throwError "CsoundTrans.topError"
               (printf "csound codegen - %s at the toplevel, cannot be translated."
                       cstr)

compValue :: S.Value -> Compiler CompValue
compValue (S.RelV op v1 v2) = CompValue op <$> value v1 <*> value v2
compValue _                 = 
    throwError "CsoundTrans.compValue"
               "csound codegen - expecting Value to be exactly a relational expression."


valDecl :: VarId -> S.Value -> Compiler Code
valDecl vid val                     = wrapH . AssignE vid <$> value val

-- | Form an assignment statement for the declaration expr of a
-- let expression (varid from the let definition supplied).
--
decl1 :: VarId -> S.Expr -> Compiler Code
decl1 vid (S.Return val)            = wrapH . AssignE vid <$> value val

decl1 vid (S.OpCall p vals)         =
    (\pc -> wrapH $ Assign1 vid pc) <$> primCall p vals

decl1 _   (S.FormRef {})            = decl1Error "form-ref"
decl1 _   (S.FunApp {})             = decl1Error "fun-app"
decl1 _   (S.LetValue {})           = decl1Error "nested let (not linearized)"
decl1 _   (S.LetSig1 {})            = decl1Error "nested letSig1"
decl1 _   (S.LetSig2 {})            = decl1Error "nested letSig2"
decl1 _   (S.LetSig3 {})            = decl1Error "nested letSig3"
decl1 _   (S.LetSig4 {})            = decl1Error "nested letSig4"
decl1 _   (S.LetTuple {})           = decl1Error "nested let-tuple (not eliminated)"
decl1 _   (S.LetFun {})             = decl1Error "nested letfun (not eliminated)"

-- Ifte must use the ternary operator when possible.
-- Push the decl into the @Ifte@ cases if not.
--
decl1 vid (S.Ifte re e1 e2)
    | simple e1 && simple e2        =
        (\cond ea eb -> wrapH $ AssignE vid (CondV cond ea eb))
          <$> compValue re <*> simpleExpr e1 <*> simpleExpr e2
    | otherwise                     =
        (\cond c1 c2 -> wrapH $ Ifte cond (toListH c1) (toListH c2))
          <$> compValue re <*> decl1 vid e1 <*> decl1 vid e2

decl1 _   (_ S.:>> _)               = decl1Error "sequence"

decl1Error :: String -> Compiler a
decl1Error cstr =
    throwError "CsoundTrans.decl1Error"
               (printf "csound codegen - %s at the declaration site, cannot be translated."
                       cstr)

-- | decl2 only expects Prim call or an if with prim calls 
-- inside.
--
decl2 :: VarId -> VarId -> S.Expr -> Compiler Code
decl2 _  _  (S.Return {})               = 
   decl2Error "single value (tuples not permitted)"

decl2 v1 v2 (S.OpCall p vals)           = 
    (\pc -> wrapH $ Assign2 v1 v2 pc) <$> primCall p vals

decl2 v1 v2 (S.Ifte cv e1 e2)           = 
    (\cond c1 c2 -> wrapH $ Ifte cond (toListH c1) (toListH c2))
      <$> compValue cv <*> decl2 v1 v2 e1 <*> decl2 v1 v2 e2

decl2 _  _  (S.FormRef {})              = decl2Error "form-ref"
decl2 _  _  (S.FunApp {})               = decl2Error "fun-app"
decl2 _  _  (S.LetValue {})             = decl2Error "letv"
decl2 _  _  (S.LetSig1 {})              = decl2Error "letSig1"
decl2 _  _  (S.LetSig2 {})              = decl2Error "letSig2"
decl2 _  _  (S.LetSig3 {})              = decl2Error "letSig3"
decl2 _  _  (S.LetSig4 {})              = decl2Error "letSig4"
decl2 _  _  (S.LetTuple {})             = decl2Error "letTuple"
decl2 _  _  (S.LetFun {})               = decl2Error "letfun"
decl2 _  _  (_ S.:>> _)                 = decl2Error "sequence"



decl2Error :: String -> Compiler a
decl2Error cstr = 
    throwError "CsoundTrans.decl2Error"
               (printf "csound codegen - %s at the declaration site of let2, cannot be translated."
                       cstr)

-- | decl3 as per decl2.
--
decl3 :: VarId -> VarId -> VarId -> S.Expr -> Compiler Code
decl3 _  _  _  (S.Return {})        = 
    decl3Error "single value (tuples not permitted)"

decl3 v1 v2 v3 (S.OpCall p vals)    = 
    (\pc -> wrapH $ Assign3 v1 v2 v3 pc) <$> primCall p vals

decl3 v1 v2 v3 (S.Ifte cv e1 e2)    = 
    (\cond c1 c2 -> wrapH $ Ifte cond (toListH c1) (toListH c2))
      <$> compValue cv <*> decl3 v1 v2 v3 e1 <*> decl3 v1 v2 v2 e2

decl3 _  _  _  (S.FormRef {})       = decl3Error "form-ref"
decl3 _  _  _  (S.FunApp {})        = decl3Error "fun-app"
decl3 _  _  _  (S.LetValue {})      = decl3Error "letv"
decl3 _  _  _  (S.LetSig1 {})       = decl3Error "letSig1"
decl3 _  _  _  (S.LetSig2 {})       = decl3Error "letSig2"
decl3 _  _  _  (S.LetSig3 {})       = decl3Error "letSig3"
decl3 _  _  _  (S.LetSig4 {})       = decl3Error "letSig4"
decl3 _  _  _  (S.LetTuple {})      = decl3Error "letTuple"
decl3 _  _  _  (S.LetFun {})        = decl3Error "letfun"
decl3 _  _  _  (_ S.:>> _)          = decl3Error "sequence"


decl3Error :: String -> Compiler a
decl3Error cstr = 
    throwError "CsoundTrans.decl3Error"
               (printf "csound codegen - %s at the declaration site of let3, cannot be translated."
                       cstr)


-- | decl4 only expects a Prim call of Ifte.
--
decl4 :: VarId -> VarId -> VarId -> VarId -> S.Expr -> Compiler Code
decl4 _  _  _  _  (S.Return {})     = 
   decl4Error "single value (tuples not permitted)"

decl4 v1 v2 v3 v4 (S.OpCall p vals) = 
    (\pc -> wrapH $ Assign4 v1 v2 v3 v4 pc) <$> primCall p vals

decl4 v1 v2 v3 v4 (S.Ifte cv e1 e2) = 
    (\cond c1 c2 -> wrapH $ Ifte cond (toListH c1) (toListH c2))
      <$> compValue cv <*> decl4 v1 v2 v3 v4 e1 <*> decl4 v1 v2 v2 v4 e2

decl4 _  _  _  _  (S.FormRef {})    = decl4Error "form-ref"
decl4 _  _  _  _  (S.FunApp {})     = decl4Error "fun-app"
decl4 _  _  _  _  (S.LetValue {})   = decl4Error "letv"
decl4 _  _  _  _  (S.LetSig1 {})    = decl4Error "letSig1"
decl4 _  _  _  _  (S.LetSig2 {})    = decl4Error "letSig2"
decl4 _  _  _  _  (S.LetSig3 {})    = decl4Error "letSig3"
decl4 _  _  _  _  (S.LetSig4 {})    = decl4Error "letSig4"
decl4 _  _  _  _  (S.LetTuple {})   = decl4Error "letTuple"
decl4 _  _  _  _  (S.LetFun {})     = decl4Error "letfun"
decl4 _  _  _  _  (_ S.:>> _)       = decl4Error "sequence"



decl4Error :: String -> Compiler a
decl4Error cstr = 
    throwError "CsoundTrans.decl4Error" 
               (printf "csound codegen - %s at the declaration site of let4, cannot be translated." 
                       cstr)

-- | Evaluate an expression for its effect (prim call like @out@
-- or @print@ or writing a global var) the expression is expected 
-- to have type Unit.
--
-- Note - whilst obviously being effects to a functional 
-- programmer, MakeRef and ReadRef and not considered effects for 
-- the translation as they only make sense as part of binding 
-- forms.
--
effect :: S.Expr -> Compiler Code
effect (S.Return {})            = effectError "value"
effect (S.OpCall p vals)        = 
    (\pc -> wrapH $ OpCall pc) <$> primCall p vals

effect (S.FormRef {})           = effectError "form-ref"
effect (S.FunApp {})            = effectError "fun-app"
effect (S.LetValue {})          = effectError "binding letv"
effect (S.LetSig1 {})           = effectError "binding letSig1"
effect (S.LetSig2 {})           = effectError "binding letSig2"
effect (S.LetSig3 {})           = effectError "binding letSig3"
effect (S.LetSig4 {})           = effectError "binding letSig4"
effect (S.LetTuple {})          = effectError "binding letTuple"
effect (S.LetFun {})            = effectError "binding letfun"
effect (S.Ifte cv e1 e2)        = 
    (\cond c1 c2 -> wrapH $ Ifte cond (toListH c1) (toListH c2))
      <$> compValue cv <*> effect e1 <*> effect e2

effect (e1 S.:>> e2)            = appendH <$> effect e1 <*> effect e2

effectError :: String -> Compiler a
effectError cstr = 
    throwError "CsoundTrans.effectError"
               (printf "csound codegen - %s found when expecting an effect, cannot be translated."
                       cstr)


primCall :: Ident -> [S.Value] -> Compiler PrimCall
primCall p vs = PrimCall p <$> mapM value vs

funCall :: Ident -> [S.Value] -> Compiler Value
funCall p vs = FunCallV p <$> mapM value vs


-- | Simple expressions can be placed as the @then@ or @else@ cases
-- in a ternary expression.

simple :: S.Expr -> Bool
simple (S.Return {})         = True 
simple _                     = False

simpleExpr :: S.Expr -> Compiler Value
simpleExpr (S.Return v)      = value v
simpleExpr _                 = 
    throwError "CsoundTrans.simpleExpr" 
               "csound codegen - only values can be translated to simple expressions."




value :: S.Value -> Compiler Value
value (S.Unit)                  = valueError "Unit"
value (S.Bool b)                = return $ if b then Int 1 else Int 0
value (S.Int i)                 = return $ Int i
value (S.Float d)               = return $ Float d
value (S.String s)              = return $ String s
value (S.Var var)               = return $ Var $ varName var
value (S.FormRefV {})           = valueError "FormRefV"
value (S.Pfield i)              = return $ Pfield i
value (S.Tuple _)               = valueError $ "Tuple"
value (S.UnaryV op v)           = UnaryV op <$> value v
value (S.BinV op v1 v2)         = BinV op <$> value v1 <*> value v2
value (S.RelV op v1 v2)         = 
    (\a b -> CondV (CompValue op a b) (Int 1) (Int 0))
      <$> value v1 <*> value v2

value (S.CondV va vt vf)        = condValue va vt vf
value (S.FunCallV pid args)     = funCall pid args


-- | Constant folding should have eliminated Booleans
-- Ints and Floats in the cond 
condValue :: S.Value -> S.Value -> S.Value -> Compiler Value
condValue (S.RelV op v1 v2)   vt vf =  
    (\a b t f -> CondV (CompValue op a b) t f) 
      <$> value v1 <*> value v2 <*> value vt <*> value vf

condValue (S.Unit {})         _  _  = condValueError "Unit"
condValue (S.Bool {})         _  _  = valueError "cond(Bool)"
condValue (S.Int {})          _  _  = valueError "cond(Int)"
condValue (S.Float {})        _  _  = valueError "cond(Float)"
condValue (S.String {})       _  _  = condValueError "String"

condValue (S.Var vid)         vt vf =
    (\t f -> manufactureCond (Var $ varName vid) t f)
      <$> value vt <*> value vf

condValue (S.FormRefV {})     _  _  = condValueError "FormRef"
condValue (S.Pfield {})       _  _  = condValueError "Pfield"

condValue (S.Tuple _)         _  _  = condValueError "Tuple"

condValue (S.UnaryV op v)     vt vf = 
    (\a t f -> manufactureCond (UnaryV op a) t f)
      <$> value v <*> value vt <*> value vf

condValue (S.BinV op v1 v2)   vt vf = 
    (\a b t f -> manufactureCond (BinV op a b) t f)
      <$> value v1 <*> value v2 <*> value vt <*> value vf

condValue (S.CondV {})        _  _ = condValueError "nested cond"

condValue (S.FunCallV pid vs) vt vf = 
    (\args t f -> manufactureCond (FunCallV pid args) t f)
      <$> mapM value vs <*> value vt <*> value vf


manufactureCond :: Value -> Value -> Value -> Value
manufactureCond cond = CondV (CompValue NEQU cond (Int 0))

condValueError :: String -> Compiler a
condValueError cstr = 
    throwError "CoundTrans.condValueError"
               (printf "csound codegen - %s not translatable as a conditional."
                       cstr)


valueError :: String -> Compiler a
valueError cstr =
    throwError "CsountTrans.valueError"
               (printf  "csound codegen - %s not eliminated during translated."
                        cstr)


