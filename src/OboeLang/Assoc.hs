{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Assoc
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- /Linearize/ nested lets to make them like Scheme\'s @let*@.
--
--------------------------------------------------------------------------------

module OboeLang.Assoc
  (
    
    assoc

  ) where


import OboeLang.CompilerMon
import OboeLang.Syntax

import Control.Applicative


--------------------------------------------------------------------------------
-- nested let linearization



assoc :: Expr -> Compiler Expr
assoc e = ll e




ll :: Expr -> Compiler Expr
ll (LetValue vid val e1)        = LetValue vid val <$> ll e1

ll (LetSig1 v1 e1 e2)           = ll e1 >>= insert
  where
    insert (LetSig1 v2 e3 e4)       = LetSig1 v2 e3 <$> insert e4
    insert (LetValue v2 val e3)     = LetSig1 v2 (Return val) <$> insert e3
    insert e                        = LetSig1 v1 e <$> ll e2

--
-- For the LetSigN elimination forms we assume they are present
-- in the syntax only at applications of multiple return
-- value opcodes (and tuples have been eliminated already). 
-- Thus we maintain them, let-linearizing only the expressions.
--
-- This assumption might need validating...
--
ll (LetSig2 v1 v2 e1 e2)        = LetSig2 v1 v2       <$> ll e1 <*> ll e2
ll (LetSig3 v1 v2 v3 e1 e2)     = LetSig3 v1 v2 v3    <$> ll e1 <*> ll e2
ll (LetSig4 v1 v2 v3 v4 e1 e2)  = LetSig4 v1 v2 v3 v4 <$> ll e1 <*> ll e2

ll (Ifte v1 e1 e2)              = Ifte v1 <$> ll e1 <*> ll e2 

ll (e1 :>> e2)                  = (:>>) <$> ll e1 <*> ll e2 

ll e                            = return e

