{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.SyntaxCommon
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common syntax.
--
--------------------------------------------------------------------------------

module OboeLang.SyntaxCommon
  (

    Decimal
  , RateId(..)
  , rateName


  -- * Identifiers
  , Ident(..)

  , ModuleName
  , makeModuleName
  , makeModuleNameL
  , deconsModuleName
  , ppModuleName
  , moduleNameString
  , moduleNameIdent

  , VarId(..)
  , makeFinal
  , ppVarId
  , varName
  , varRate

  -- * Operators
  , UnaryOp(..)
  , BinOp(..)
  , RelOp(..)
  , unaryOpName
  , binOpName
  , relOpName

  -- * Pretty print
  , unaryDocE
  , binDocE
  , relDocE

  ) where

import OboeLang.Utils.Pretty
import OboeLang.Utils.PrettyExprH98
import OboeLang.Utils.PrettyExprHPJ

import Text.PrettyPrint.HughesPJ                -- package: pretty

import Data.List.Split ( splitOn )              -- package: split

import Data.Fixed
import Data.List ( intercalate )


type Decimal = Fixed E9 


data RateId = I_RATE | K_RATE | A_RATE
  deriving (Enum,Eq,Ord,Show)


rateName :: RateId -> Char
rateName I_RATE = 'i'
rateName K_RATE = 'k'
rateName A_RATE = 'a'

-- | Note - Ident will not get alpha renamed, think carefully where
-- it is used.
--
-- Function names should use Ident.
--
newtype Ident = Ident { getIdent :: String }
  deriving (Eq,Ord,Show)


instance Pretty Ident where
  pretty = text . getIdent



data ModuleName = ModuleName 
    { module_scoped_prefix :: [String]
    , module_last          :: String
    }
  deriving (Eq,Ord,Show)



makeModuleName :: String -> Maybe ModuleName
makeModuleName = makeModuleNameL . splitOn "."


makeModuleNameL :: [String] -> Maybe ModuleName
makeModuleNameL [] = Nothing 
makeModuleNameL (x:xs) = Just $ build [] x xs
  where
    build ac ac1 []     = ModuleName ac ac1
    build ac ac1 (y:ys) = build (ac++[ac1]) y ys

deconsModuleName :: ModuleName -> ([String], String)
deconsModuleName a = (module_scoped_prefix a, module_last a)



ppModuleName :: ModuleName -> Doc
ppModuleName = text . moduleNameString 


moduleNameString :: ModuleName -> String
moduleNameString = expand . merge . deconsModuleName
  where
    merge (xs,x) = xs ++ [x]
    expand       = intercalate "."

moduleNameIdent :: ModuleName -> Ident
moduleNameIdent = Ident . moduleNameString


-- | Final variables do not get Alpha remaned - use with caution.
--
-- LamNames get alpha renamed
--
data VarId = Final    { var_rate  :: RateId
                      , var_name  :: String
                      }
           | Local    { var_rate  :: RateId
                      , var_num   :: Int
                      }
           | LamName  { var_num :: Int }
  deriving (Eq,Ord,Show)


-- | TODO - globals?
makeFinal :: String -> Maybe VarId
makeFinal s@('i':_) = Just $ Final { var_rate = I_RATE, var_name = s }
makeFinal s@('k':_) = Just $ Final { var_rate = K_RATE, var_name = s }
makeFinal s@('a':_) = Just $ Final { var_rate = A_RATE, var_name = s }
makeFinal _         = Nothing

-- | Identifiers whose type cannot be rendered to Csound 
-- prim-types get name mangled.
--
varName :: VarId -> String
varName (Final _ ss)    = ss
varName (Local ri i)    = rateName ri : show i
varName (LamName i)     = "LAM" ++ show i

varRate :: VarId -> Maybe RateId
varRate (Final ri _) = Just ri
varRate (Local ri _) = Just ri
varRate (LamName {}) = Nothing



ppVarId :: VarId -> Doc
ppVarId = text . varName




--------------------------------------------------------------------------------
-- Operators


-- | Negation and Bitwise NOT.
--
data UnaryOp = NEG | BNOT
  deriving (Eq,Show)


-- | No power of @(^)@ as its use is discouraged.
--
data BinOp = ADD | SUB | MUL | DIV | LOG_AND | LOG_OR | MODULUS 
  deriving (Enum,Eq,Ord,Show)

data RelOp = EQU | NEQU | LESSTHAN | GREATTHAN | LTEQU | GTEQU
  deriving (Enum,Eq,Ord,Show)


unaryOpName :: UnaryOp -> String
unaryOpName NEG         = "-"
unaryOpName BNOT        = "~"

binOpName :: BinOp -> String
binOpName ADD           = "+"
binOpName SUB           = "-"
binOpName MUL           = "*"
binOpName DIV           = "/"
binOpName LOG_AND       = "&&" 
binOpName LOG_OR        = "||"
binOpName MODULUS       = "%"

relOpName :: RelOp -> String
relOpName EQU           = "=="
relOpName NEQU          = "!="
relOpName LESSTHAN      = "<"
relOpName GREATTHAN     = ">"
relOpName LTEQU         = "<="
relOpName GTEQU         = ">="

--------------------------------------------------------------------------------
-- Pretty print


unaryDocE :: UnaryOp -> DocE -> DocE
unaryDocE NEG   = negateU
unaryDocE BNOT  = bitwiseNotU


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


-- | Precedence higher than comparisons lower than arithmetic.
--
bitwiseNotU             :: DocE -> DocE
bitwiseNotU             = Unary (prefix 5) (noSpace "~")

