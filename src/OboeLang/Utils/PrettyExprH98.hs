{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Utils.PrettyExprH98
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common Haskell operator and expression pretty printers with 
-- associated fixity and associativity. 
--
-- Note - these are pretty printers following the definition of
-- fixity and associativity in the Haskell 98 standard. This 
-- module does not attempt to be a pretty print /for all of/ 
-- Haskell 98.
--
--------------------------------------------------------------------------------

module OboeLang.Utils.PrettyExprH98
  (

  -- * Re-export DocE
    DocE(..)

  -- * Unparse a DocE
  , unparse

  -- * Literals 
  , literal             -- re-export

  -- * Haskell operators and expressions

  -- ** Precedence 10
  , funAppB1
  , funAppB


  -- ** Precedence 9
  , listIndexB
  , composeB
  , negateU

  -- ** Precedence 8
  , powerOfB
  , powerOfFracB
  , expLogB

  -- ** Precedence 7
  , multiplyB  
  , divideB
  , divB
  , modB
  , remB
  , quotB

  -- ** Precedence 6
  , addB
  , subtractB 

  -- ** Precedence 5
  , consB
  , concatB

  -- ** Precedence 4
  , equalB
  , notEqualB
  , lessThanB
  , lessThanEqB
  , greaterThanB
  , greaterThanEqB
  , elemB
  , notElemB


  -- * Precedence 3
  , logicalAndB

  -- * Precedence 2
  , logicalOrB

  -- * Precedence 1
  , monadicSeqB
  , monadicBindB

  -- * Precedence 0
  , applyB
  , strictApplyB
  , seqB

  ) where

import OboeLang.Utils.PrettyExprHPJ

import Text.PrettyPrint.HughesPJ            -- package: pretty


-- | Run an unparser for Haskell-like language expressions.
-- 
-- Haskell has maximum precedence of 10 (function application
-- has precedence one more the the highest operator 9).
--
unparse :: DocE -> Doc
unparse = unparser $ makeUnparser 11        -- We want 1+ highest level



--------------------------------------------------------------------------------
-- Precedence 10


funAppB1                :: DocE -> DocE -> DocE
funAppB1                = Binary (infixL 10) (noSpace " ")

funAppB                 :: DocE -> [DocE] -> DocE
funAppB d []            = d
funAppB d [e]           = d `funAppB1` e
funAppB d (e:es)        = d `funAppB1` e `funAppB` es


{-

-- | Note - @let decl in expr@ has a precedence of 10 but we 
-- currently can\'t model this with Atom...

letIn                   :: Doc -> DocE -> DocE
letIn decls body        = atomPrec 10 $ 
        text "let" <+> decls
    $+$ text "in"  <+> unparse body
-}

--------------------------------------------------------------------------------
-- Precedence 9

-- | List indexing operator.
--
-- > !! (infixl 9)
--
listIndexB              :: DocE -> DocE -> DocE
listIndexB              = Binary (infixL 9) (noSpace "!!")


-- | Function composition operator.
-- 
-- > . (infixr 9)
--
composeB                :: DocE -> DocE -> DocE
composeB                = Binary (infixR 9) (spaced ".")




-- | Unary negation, textual representation.
-- 
-- > negate (unary)
--
negateU                 :: DocE -> DocE
negateU                 = Unary (prefix 9) (spaced "negate")

--------------------------------------------------------------------------------
-- Precedence 8

-- | Power-of operator (Num constraint), non-negative.
-- 
-- > ^ (infixr 8)
--
powerOfB                :: DocE -> DocE -> DocE
powerOfB                = Binary (infixR 8) (spaced "^")


-- | Power-of operator (Fractional constraint), negative powers 
-- allowed .
-- 
-- > ^^ (infixr 8)
--
powerOfFracB            :: DocE -> DocE -> DocE
powerOfFracB            = Binary (infixR 8) (spaced "^^")


-- | Exponent log (?) operator.
--
--
-- > ** (infixr 8)
--
-- > x ** y == exp (log x * y)
--  
expLogB                 :: DocE -> DocE -> DocE
expLogB                 = Binary (infixR 8) (spaced "**")

--------------------------------------------------------------------------------
-- Precedence 7

-- | Multiplication operator.
-- 
-- > * (infixl 7)
--
multiplyB               :: DocE -> DocE -> DocE
multiplyB               = Binary (infixL 7) (spaced "*")


-- | Division operator (Fractional).
-- 
-- > / (infixl 7)
--
divideB                 :: DocE -> DocE -> DocE
divideB                 = Binary (infixL 7) (spaced "/")


-- | Division operator, (Int) textual representation.
-- 
-- > `div` (infixl 7)
--
divB                    :: DocE -> DocE -> DocE
divB                    = Binary (infixL 7) (spaced "`div`")


-- | Modulus operator, textual representation.
-- 
-- > `mod` (infixl 7)
--
modB                    :: DocE -> DocE -> DocE
modB                    = Binary (infixL 7) (spaced "`mod`")


-- | Remainder operator, textual representation.
-- 
-- > `rem` (infixl 7)
--
remB                    :: DocE -> DocE -> DocE
remB                    = Binary (infixL 7) (spaced "`rem`")

-- | Quotient operator, textual representation.
-- 
-- > `quot` (infixl 7)
--
quotB                   :: DocE -> DocE -> DocE
quotB                   = Binary (infixL 7) (spaced "`quot`")


--------------------------------------------------------------------------------
-- Precedence 6


-- | Addition operator.
-- 
-- > + (infixl 6)
--
addB                    :: DocE -> DocE -> DocE
addB                    = Binary (infixL 6) (spaced "+")


-- | Subtraction operator.
-- 
-- > - (infixl 6)
--
subtractB               :: DocE -> DocE -> DocE
subtractB               = Binary (infixL 6) (spaced "-")

--------------------------------------------------------------------------------
-- Precedence 5

-- | Cons operator (Lists).
-- 
-- > : (infixr 5)
--
consB                   :: DocE -> DocE -> DocE
consB                   = Binary (infixR 5) (noSpace ":")

-- | Concatenation operator (Lists).
-- 
-- > ++ (infixr 5)
--
concatB                 :: DocE -> DocE -> DocE
concatB                 = Binary (infixR 5) (noSpace "++")


--------------------------------------------------------------------------------
-- Precedence 4


-- | Equality testing operator (non-associative).
-- 
-- > == (infix 4)
--
equalB                  :: DocE -> DocE -> DocE
equalB                  = Binary (infixNonAssoc 4) (spaced "==")

-- | Inequality testing operator (non-associative).
-- 
-- > /= (infix 4)
--
notEqualB               :: DocE -> DocE -> DocE
notEqualB               = Binary (infixNonAssoc 4) (spaced "/=")


-- | Less than operator (non-associative).
-- 
-- > < (infix 4)
--
lessThanB               :: DocE -> DocE -> DocE
lessThanB               = Binary (infixNonAssoc 4) (spaced "<")


-- | Less than or equal operator (non-associative).
-- 
-- > <= (infix 4)
--
lessThanEqB             :: DocE -> DocE -> DocE
lessThanEqB             = Binary (infixNonAssoc 4) (spaced "<=")

-- | Greater than operator (non-associative).
-- 
-- > > (infix 4)
--
greaterThanB            :: DocE -> DocE -> DocE
greaterThanB            = Binary (infixNonAssoc 4) (spaced ">")


-- | Greater than or equal operator (non-associative).
-- 
-- > >= (infix 4)
--
greaterThanEqB          :: DocE -> DocE -> DocE
greaterThanEqB          = Binary (infixNonAssoc 4) (spaced ">=")


-- | Elem - list membership operator, textual representation.
-- 
-- > `elem` (infix 4)
--
elemB                   :: DocE -> DocE -> DocE
elemB                   = Binary (infixNonAssoc 4) (spaced "`elem`")

-- | notElem - list non-membership operator, textual 
-- representation.
-- 
-- > `notElem` (infix 4)
--
notElemB                :: DocE -> DocE -> DocE
notElemB                = Binary (infixNonAssoc 4) (spaced "`notElem`")

--------------------------------------------------------------------------------
-- Precedence 3

-- | Logical and operator.
-- 
-- > && (infixr 3)
--
logicalAndB             :: DocE -> DocE -> DocE
logicalAndB             = Binary (infixR 3) (spaced "&&")



--------------------------------------------------------------------------------
-- Precedence 2

-- | Logical or operator.
-- 
-- > || (infixr 2)
--
logicalOrB              :: DocE -> DocE -> DocE
logicalOrB              = Binary (infixR 2) (spaced "||")


--------------------------------------------------------------------------------
-- Precedence 1


-- | Monadic sequencing operator.
-- 
-- > >> (infixl 1)
--
monadicSeqB             :: DocE -> DocE -> DocE
monadicSeqB             = Binary (infixL 1) (spaced ">>")


-- | Monadic bind operator.
-- 
-- > >>= (infixl 1)
--
monadicBindB            :: DocE -> DocE -> DocE
monadicBindB            = Binary (infixL 1) (spaced ">>=")

--------------------------------------------------------------------------------
-- Precedence 0

-- | Function application operator.
-- 
-- > $ (infixr 0)
--
applyB                  :: DocE -> DocE -> DocE
applyB                  = Binary (infixR 0) (spaced "$")


-- | Strict function application operator.
-- 
-- > $! (infixr 0)
--
strictApplyB            :: DocE -> DocE -> DocE
strictApplyB            = Binary (infixR 0) (spaced "$!")


-- | seq binary operator - forces left-hand side, returns
-- right-hand side.
-- 
-- > `seq` (infixr 0)
--
seqB                    :: DocE -> DocE -> DocE
seqB                    = Binary (infixR 0) (spaced "`seq`")





