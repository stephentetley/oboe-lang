{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Utils.Common
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Utils.
--
--------------------------------------------------------------------------------

module OboeLang.Utils.Common
  (

  -- * Monadic combinators
    repeatM
  , passM
  , debugM 

  -- * Showing numbers
  , truncatedDouble


  -- * Hughes list
  , H  
  , emptyH
  , wrapH
  , consH
  , snocH
  , appendH
  , toListH

  ) where


import Numeric


--------------------------------------------------------------------------------
-- Monadic combinators

repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM count mf = go count
  where
    go i a | i <= 0    = return a
           | otherwise = mf a >>= go (i-1)



passM :: Monad m => (a -> m z) -> a -> m a
passM mz a = mz a >> return a

debugM :: Monad m => (a -> m b) -> (b -> m ()) -> a -> m b
debugM mf mz a = mf a >>= \b -> mz b >> return b


--------------------------------------------------------------------------------
-- Showing numbers


showFFloat_ :: RealFloat a => a -> String
showFFloat_ = ($ "") . showFFloat Nothing


-- | Truncate the printed decimal representation of a Double.
-- This may be prefered to 'showFFloat' from Numeric as it 
-- produces shorter representations when the value has trailing
-- zeros.
-- 
-- 0.000000000 becomes 0.0 rather than however many digits are 
-- specified.
--  
truncatedDouble :: Double -> String
truncatedDouble d 
    | abs d < 0.0001  = "0.0"
    | d < 0.0         = '-' :  showFFloat_ (abs tx)
    | otherwise       = showFFloat_ tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0

roundi :: RealFrac a => a -> Integer
roundi = round

--------------------------------------------------------------------------------
-- Hughes list

type H a = [a] -> [a]

emptyH :: H a
emptyH = id

wrapH :: a -> H a
wrapH a = (a:)

consH :: a -> H a -> H a
consH a h = (a:) . h

snocH :: H a -> a -> H a
snocH h a = h . (a:)

appendH :: H a -> H a-> H a
appendH = (.)

toListH :: H a -> [a]
toListH h = h []

