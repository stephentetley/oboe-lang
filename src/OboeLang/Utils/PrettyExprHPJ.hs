{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Utils.PrettyExprHPJ
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pretty print expressions with infix, prefix and postfix
-- operators.
--
--------------------------------------------------------------------------------

module OboeLang.Utils.PrettyExprHPJ
  (

    -- * Types
    Assoc(..)
  , Fixity(..)
  , Precedence
  , Spacing(..)
  , Rator
  , DocE(..)

  -- * Specifying operators
  , prefix
  , postfix
  , infixL
  , infixR
  , infixNonAssoc

  -- * Specifying symbols
  , spaced
  , noSpace

  -- * Useful 
  , literal

  -- * Turn a DocE into a Doc with an Unparser
  , Unparser(..)
  , makeUnparser

  ) where


import Text.PrettyPrint.HughesPJ hiding ( cat )       -- package: pretty

import Data.List ( foldl') 


-- | Specify the associativity of operators: left, right or
-- none:
--
-- > NON_ASSOC
-- > ASSOC_LEFT
-- > ASSOC_RIGHT
--
data Assoc = NON_ASSOC 
           | ASSOC_LEFT
           | ASSOC_RIGHT
  deriving (Bounded, Enum, Eq, Ord, Show)


-- | Specify the fixity of an operator, either prefix, postfix
-- or infix. 
--
-- INFIX operators have an associated associativity.
--
data Fixity = PREFIX 
            | POSTFIX 
            | INFIX Assoc
  deriving (Eq,Ord,Show)

-- | Precedence level of an operator - this is just a synonym for 
-- 'Int'.
--
type Precedence = Int


data Spacing = NO_SPACE
             | SPACED
  deriving (Bounded, Enum, Eq, Ord, Show)


-- | String is the literal to print.
--
type Symbol = (String,Spacing)

type Rator = (Precedence, Fixity)




data DocE = Atom Doc
          | Binary Rator Symbol DocE DocE
          | Unary Rator Symbol DocE
          | Ternary Rator Symbol Symbol DocE DocE DocE
          | Nary Rator Symbol [DocE]

-- No show instance



-- | Specify a prefix operator, supplying its precedence, spacing 
-- and its representation as a String.
--
prefix :: Precedence -> Rator 
prefix i = (i,PREFIX)


-- | Specify a postfix operator, supplying its precedence.
--
postfix :: Precedence -> Rator 
postfix i = (i,POSTFIX)

-- | Specify a left-associative, infix operator, supplying its 
-- precedence.
--
infixL :: Precedence -> Rator 
infixL i = (i, INFIX ASSOC_LEFT)

-- | Specify a right-associative, infix operator, supplying its 
-- precedence.
--
infixR :: Precedence -> Rator 
infixR i = (i, INFIX ASSOC_RIGHT)

-- | Specify a none-associative, infix operator, supplying its 
-- precedence.
--
infixNonAssoc :: Precedence -> Rator 
infixNonAssoc i = (i, INFIX NON_ASSOC)


-- | Specify a symbol printed with surrounding whitespace.
--
spaced :: String -> Symbol 
spaced s = (s,SPACED)

-- | Specify a symbol tightly printed with no surrounding 
-- whitespace.
--
noSpace :: String -> Symbol 
noSpace s = (s,NO_SPACE)



-- | Local helper - ordering based on second element of the
-- tuple.
--
higherPrec :: Rator -> Rator -> Bool
higherPrec (i,_) (j,_) = i > j

-- | Local helper.
--
equalPrec :: Rator -> Rator -> Bool
equalPrec (i,_) (j,_) = i == j

isPostfix :: Rator -> Bool
isPostfix = (== POSTFIX) . snd

isPrefix :: Rator -> Bool
isPrefix = (== PREFIX) . snd


fixity :: Rator -> Fixity
fixity = snd


cat :: Symbol -> (Doc -> Doc -> Doc)
cat (_,NO_SPACE) = (<>)
cat (_,SPACED)   = (<+>)

assertFixity2 :: Fixity -> Rator -> Rator -> Bool
assertFixity2 fx (_,a) (_,b) = fx == a && fx == b



ppInfix :: Doc -> Symbol -> Doc -> Doc
ppInfix a op@(name,_) b = a @@ text name @@ b
  where
    (@@) = cat op


ppTernary :: Doc -> Symbol -> Doc -> Symbol -> Doc -> Doc
ppTernary a op@(name1,_) b (name2,_) c = 
    a @@ text name1 @@ b @@ text name2 @@ c
  where
    (@@) = cat op


ppPrefix :: Symbol -> Doc -> Doc
ppPrefix  op@(name,_) a = text name @@ a
  where
    (@@) = cat op

ppPostfix :: Doc -> Symbol -> Doc
ppPostfix a op@(name,_) = a @@ text name
  where
    (@@) = cat op



-- equivalent to @showParen@ for ShowS
--
docParen :: Bool -> Doc -> Doc
docParen cond = if cond then parens else id


type Fragment = (Rator, Doc)  

bracket :: Fragment -> Assoc -> Rator -> Doc
bracket (kid,doc) assoc parent = docParen (not $ predicate assoc) doc
  where
    predicate ASSOC_LEFT  = noparensLeft  kid parent
    predicate ASSOC_RIGHT = noparensRight kid parent
    predicate NON_ASSOC   = noparensUnary kid parent



noparensUnary :: Rator -> Rator -> Bool
noparensUnary kid parent
    | higherPrec kid parent = True
    | otherwise             = fixity kid == fixity parent

noparensLeft :: Rator -> Rator -> Bool
noparensLeft kid parent     
    | higherPrec kid parent = True
    | isPostfix kid         = True
    | equalPrec kid parent  = assertFixity2 (INFIX ASSOC_LEFT) kid parent
    | otherwise             = False


noparensRight :: Rator -> Rator -> Bool
noparensRight kid parent     
    | higherPrec kid parent = True
    | isPrefix kid          = True
    | equalPrec kid parent  = assertFixity2 (INFIX ASSOC_RIGHT) kid parent
    | otherwise             = False


--------------------------------------------------------------------------------
-- Utility functions


literal                 :: String -> DocE
literal                 = Atom . text


--------------------------------------------------------------------------------

-- | An Unparser is a so-called /first-class module/ where
-- the maximum precedence level is configurable.
-- 
-- Internally an unparser needs to know the maximum precedence
-- level which may vary - for Haskell it is, 10, (depending
-- how you count!) for C it is 16.
--
-- Instead of using a reader monad to supply this information
-- and Unparser is a /module/ instantatiated with the maximum 
-- precedence.
-- 
-- This pattern is used by Parsec\'s Token module for instance.
-- 
data Unparser = Unparser { unparser :: DocE -> Doc }


-- | Create an Unparser with the supplied maximum precedence.
--
makeUnparser :: Precedence -> Unparser
makeUnparser maxprec = Unparser { unparser = unP }
  where
    maxrator :: Rator
    maxrator =  (maxprec, INFIX NON_ASSOC)

    unP :: DocE -> Doc      
    unP = snd . go
      where
        go (Atom a)                     = (maxrator,a)
 
        go (Unary rat op a)             = 
            fn (fixity rat) $ bracket (go a) NON_ASSOC rat
          where
            fn PREFIX    e  = (rat, ppPrefix op e)
            fn POSTFIX   e  = (rat, ppPostfix e op)
            fn (INFIX _) _  = error "makeUnparser - imposible"

        go (Binary rat op a b)          = (rat, ppInfix l op r)
          where
            l = bracket (go a) ASSOC_LEFT  rat
            r = bracket (go b) ASSOC_RIGHT rat

        go (Ternary rat op1 op2 a b c)  = (rat, ppTernary l op1 m op2 r)
          where
            l = bracket (go a) ASSOC_LEFT  rat
            m = bracket (go b) NON_ASSOC   rat
            r = bracket (go c) ASSOC_RIGHT rat

        go (Nary _   _  [])             = 
            error "unparse - Nary supplied with the empty list"
        go (Nary _   _  [e])            = go e
        go (Nary rat op (e:es))         = (rat, foldl' addOne leftmost es)
          where
            leftmost        = bracket (go e) ASSOC_LEFT rat
            addOne leftp r  = ppInfix leftp op (bracket (go r) ASSOC_RIGHT rat)
