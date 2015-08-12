{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ScExprPretty
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- SuperCollider operator pretty printers.
--
-- WARNING - I don\'t (yet) understand the SuperCollider grammar 
-- very well.
--
--------------------------------------------------------------------------------

module OboeLang.ScExprPretty
  (
  -- * Unparse a DocE
    unparse

  , unaryDocE
  , binDocE
  , relDocE

  -- * Csound operators
  -- ** Precedence 15
  , negateU
  , bitwiseNotU

  -- ** Precedence 13
  , multiplyB
  , divideB

  -- * Precendence 12
  , addB
  , subtractB

  -- ** Precedence 10
  , lessThanB
  , greaterThanB
  , lessThanEqB
  , greaterThanEqB

  -- ** Precedence 9
  , equalB
  , notEqualB

  -- ** Precedence 5
  , logicalAndB


  ) where


import OboeLang.Utils.Pretty
import OboeLang.Utils.PrettyExprHPJ
import OboeLang.SyntaxBase hiding ( binDocE, unaryDocE, relDocE )





-- | Run an unparser for C-like language expressions.
-- 
-- C99 has maximum precedence of 16.
--
unparse :: DocE -> Doc
unparse = unparser $ makeUnparser 16



unaryDocE :: UnaryOp -> DocE -> DocE
unaryDocE NEG       = negateU
unaryDocE BNOT      = bitwiseNotU


binDocE :: BinOp -> DocE -> DocE -> DocE
binDocE ADD         = addB
binDocE SUB         = subtractB
binDocE MUL         = multiplyB
binDocE DIV         = divideB
binDocE LOG_AND     = logicalAndB
binDocE LOG_OR      = logicalOrB 
binDocE MODULUS     = modB

relDocE :: RelOp -> DocE -> DocE -> DocE
relDocE EQU         = equalB
relDocE NEQU        = notEqualB
relDocE LESSTHAN    = lessThanB
relDocE GREATTHAN   = greaterThanB
relDocE LTEQU       = lessThanEqB
relDocE GTEQU       = greaterThanEqB


--------------------------------------------------------------------------------
-- Precedence 15

-- | Unary negation.
-- 
-- > - (prefix 15)
--
negateU                 :: DocE -> DocE
negateU                 = Unary (prefix 15) (noSpace "-")


-- | Bitweise not. Uses tilde.
--
-- > ~ (prefix 15)
--
--
bitwiseNotU             :: DocE -> DocE
bitwiseNotU             = Unary (prefix 15) (noSpace "~")


--------------------------------------------------------------------------------
-- Precedence 13

-- | Multiplication.
-- 
-- > * (infixl 13)
--
multiplyB               :: DocE -> DocE -> DocE
multiplyB               = Binary (infixL 13) (noSpace "*")

-- | Division.
-- 
-- > / (infixl 13)
--
divideB                 :: DocE -> DocE -> DocE
divideB                 = Binary (infixL 13) (noSpace "/")


-- | Modulus.
-- 
-- > % (infixl 13)
--
modB                    :: DocE -> DocE -> DocE
modB                    = Binary (infixL 13) (noSpace "%")



--------------------------------------------------------------------------------
-- Precedence 12

-- | Addition.
-- 
-- > + (infixl 12)
--
addB                    :: DocE -> DocE -> DocE
addB                    = Binary (infixL 12) (noSpace "+")

-- | Subtraction.
-- 
-- > - (infixl 12)
--
subtractB               :: DocE -> DocE -> DocE
subtractB              = Binary (infixL 12) (noSpace "-")


--------------------------------------------------------------------------------
-- Precedence 10

-- | Less than.
-- 
-- > < (infixl 10)
--
lessThanB               :: DocE -> DocE -> DocE
lessThanB               = Binary (infixL 10) (spaced "<")


-- | Less than or equal.
-- 
-- > <= (infixl 10)
--
lessThanEqB             :: DocE -> DocE -> DocE
lessThanEqB             = Binary (infixL 10) (spaced "<=")



-- | Greater than.
-- 
-- > > (infixl 10)
--
greaterThanB            :: DocE -> DocE -> DocE
greaterThanB            = Binary (infixL 10) (spaced ">")


-- | Greater than or equal.
-- 
-- > >= (infixl 10)
--
greaterThanEqB          :: DocE -> DocE -> DocE
greaterThanEqB          = Binary (infixL 10) (spaced ">=")

--------------------------------------------------------------------------------
-- Precedence 9

-- | Equality.
-- 
-- > == (infixl 9)
--
equalB                  :: DocE -> DocE -> DocE
equalB                  = Binary (infixL 9) (spaced "==")


-- | Inequality.
-- 
-- > != (infixl 9)
--
notEqualB               :: DocE -> DocE -> DocE
notEqualB               = Binary (infixL 9) (spaced "!=")

--------------------------------------------------------------------------------
-- Precedence 5

-- | Logical and.
-- 
-- > && (infixl 5)
--
logicalAndB             :: DocE -> DocE -> DocE
logicalAndB             = Binary (infixL 5) (noSpace "&&")


--------------------------------------------------------------------------------
-- Precedence 4

-- | Logical or.
-- 
-- > || (infixl 4)
--
logicalOrB              :: DocE -> DocE -> DocE
logicalOrB              = Binary (infixL 4) (noSpace "||")

