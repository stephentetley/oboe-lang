{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ElimLet
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Useless Let elimination for intermediate repr.
--
-- Note - OboeLang.Assoc linearizes lets.
--
--------------------------------------------------------------------------------

module OboeLang.ElimLet
  (
    
    elimLet

  ) where


import OboeLang.CompilerMon
import OboeLang.Syntax
import OboeLang.SyntaxCommon

import Control.Applicative
import qualified Data.Set as Set



--------------------------------------------------------------------------------
-- Useless let elimination

anyMembers :: [VarId] -> FreeVars -> Bool
anyMembers vs fvs = any (`Set.member` fvs) vs


elimLet :: Expr -> Compiler Expr
elimLet e = elim e


-- TODO - we should avoid inlining conds
elim :: Expr -> Compiler Expr
elim (LetValue vid val e)           = 
    (\ea -> ifte (Set.member vid (freevars ea))
                 (LetValue vid val ea)
                 ea) 
     <$> elim e
                                       
elim (LetSig1 vid e1 e2)            = 
    (\ea eb -> ifte (effect ea || Set.member vid (freevars eb))
                    (LetSig1 vid ea eb) 
                    eb) 
      <$> elim e1 <*> elim e2

elim (LetSig2 v1 v2 e1 e2)          = 
    (\ea eb -> ifte (effect ea || anyMembers [v1,v2] (freevars eb))
                    (LetSig2 v1 v2 ea eb)
                    eb) 
      <$> elim e1 <*> elim e2

elim (LetSig3 v1 v2 v3 e1 e2)       = 
    (\ea eb -> ifte (effect ea || anyMembers [v1,v2,v3] (freevars eb))
                    (LetSig3 v1 v2 v3 ea eb) 
                    eb) 
      <$> elim e1 <*> elim e2

elim (LetSig4 v1 v2 v3 v4 e1 e2)    = 
    (\ea eb -> ifte (effect ea || anyMembers [v1,v2,v3,v4] (freevars eb))
                    (LetSig4 v1 v2 v3 v4 ea eb) 
                    eb) 
      <$> elim e1 <*> elim e2

elim (Ifte cond e1 e2)              = Ifte cond <$> elim e1 <*> elim e2
elim (e1 :>> e2)                    = (:>>) <$> elim e1 <*> elim e2
elim e                              = return e

ifte :: Bool -> a -> a -> a
ifte cond t f = if cond then t else f