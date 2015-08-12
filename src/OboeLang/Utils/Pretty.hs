{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Utils.Pretty
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monadic (Reader) wrapper over Text.PrettyPrint.
--
--------------------------------------------------------------------------------

module OboeLang.Utils.Pretty
  (
    Pretty(..)
  , Alignment(..)

  , padTextR

  , blank
  , vsep
  , vspaced
  , commaSep
  , tupled
  , semiBraces
  , commaBraces
  , commaBrackets

  , blockSemiBraces
  , blockSemiBraces1
  , blockCommaParens
  , blockCommaParens1


  ) where


import Text.PrettyPrint.HughesPJ hiding ( sep )     -- package: pretty


class Pretty a where 
  pretty :: a -> Doc

data Alignment = LEAD_SEP | TRAIL_SEP
  deriving (Eq,Ord,Show)


blank :: Doc
blank = zeroWidthText ""



padTextR :: Int -> String -> Doc
padTextR i ss | len <- length ss
              , len < i           = text ss <> text (replicate (i -len) ' ')
padTextR _ ss                     = text ss




vsep :: [Doc] -> Doc
vsep []     = empty
vsep [d]    = d
vsep (d:ds) = d $+$ vsep ds

vspaced :: [Doc] -> Doc
vspaced []     = empty
vspaced [d]    = d
vspaced (d:ds) = d $+$ blank $+$ vspaced ds


commaSep :: [Doc] -> Doc
commaSep []     = empty
commaSep [a]    = a
commaSep (a:as) = a <> comma <+> commaSep as


tupled :: [Doc] -> Doc
tupled ds = parens $ hcat $ punctuate comma ds

semiBraces :: [Doc] -> Doc
semiBraces = braces . hcat . punctuate semi

commaBraces :: [Doc] -> Doc
commaBraces = braces . hcat . punctuate comma

commaBrackets :: [Doc] -> Doc
commaBrackets = brackets . hcat . punctuate comma



-- Should these have choice of LEAD / TRAIL?
--
blockSemiBraces :: Alignment -> [Doc] -> Doc
blockSemiBraces a = verticals a (lbrace,rbrace) semi

blockSemiBraces1 :: Alignment -> [Doc] -> Doc
blockSemiBraces1 a = verticals1 a (lbrace,rbrace) semi

blockCommaParens :: Alignment -> [Doc] -> Doc
blockCommaParens a = verticals a (lparen,rparen) comma

blockCommaParens1 :: Alignment -> [Doc] -> Doc
blockCommaParens1 a = verticals1 a (lparen,rparen) comma


verticals :: Alignment -> (Doc,Doc) -> Doc -> [Doc] -> Doc
verticals a (start,stop) sep ds = case a of 
    LEAD_SEP -> start $+$ leadSep sep ds $+$ stop
    TRAIL_SEP -> start $+$ trailSep sep (map (nest 2) ds) $+$ stop

verticals1 :: Alignment -> (Doc,Doc) -> Doc -> [Doc] -> Doc
verticals1 _ _            _   []     = empty
verticals1 a (start,stop) sep (d:ds) = case a of 
    LEAD_SEP -> leadSep sep ((start <> d) : ds) $+$ stop
    TRAIL_SEP -> trailSep sep ((start <> d) : map (nest 2) ds) $+$ stop


leadSep :: Doc -> [Doc] -> Doc
leadSep _   []     = empty
leadSep sep (x:xs) = vcat $ x : map (sep <+>) xs


trailSep :: Doc -> [Doc] -> Doc
trailSep _   []     = empty
trailSep sep (x:xs) = vcat $ step x xs
  where
    step d []     = [d]
    step d (y:ys) = d <> sep : step y ys
