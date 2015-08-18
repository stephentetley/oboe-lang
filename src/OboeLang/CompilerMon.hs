{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.CompilerMon
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monad supplying IO (for loading modules), fresh identifiers 
-- plus error reporting and warning logging.
--
--------------------------------------------------------------------------------


module OboeLang.CompilerMon
  ( 

    ErrMsg
  , SearchPaths
  , makeSearchPaths
  , DDumpOpt(..)
  , Config
  , makeConfig

  , Log

  , Compiler
  , runCompilerMonad

  , liftIO
  , initProductiveCount
  , incrProductiveCount
  , getProductiveCount

  , initNameGen
  , fresh
  , freshLamId
  , refresh

  , askPassCount
  , askBuiltinNames
  , askSearchPaths

  , queryDDumpOpt

  , throwError

  , tellDoc
  , warning
  , ddump

  ) where

import OboeLang.Builtin
import OboeLang.SyntaxCommon
import OboeLang.Utils.Common
import OboeLang.Utils.Pretty

import Text.PrettyPrint.HughesPJ                -- package: pretty
import Data.List.Split                          -- package: split

import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set

type ErrMsg = String


newtype SearchPaths = SearchPaths { getSearchPaths :: [FilePath] }
  deriving (Eq,Show)

makeSearchPaths :: String -> SearchPaths
makeSearchPaths = SearchPaths . splitOn ":"


-- | Note - may need extending for to account for Zak \"ports\".
--
data St = St
    { ivar_count        :: !Int
    , kvar_count        :: !Int
    , avar_count        :: !Int
    , lam_count         :: !Int
    , productive_count  :: !Int
    }

state_zero :: St
state_zero = St { ivar_count        = 1
                , kvar_count        = 1
                , avar_count        = 1
                , lam_count         = 1
                , productive_count  = 0
                }


type Log = H Doc


logText :: Log -> String
logText = render . vspaced . toListH



data DDumpOpt = DDUMP_PARSED
              | DDUMP_FORMS
              | DDUMP_INTER
              | DDUMP_INLINE
              | DDUMP_INLINE_LETFUN     
              | DDUMP_ELIM_TUPLE        
              | DDUMP_OPTIMIZE_INTER
  deriving (Eq,Ord,Show)


-- | Note - builtin is not avial at init-time. 
-- Use @withBuiltins@ when the are avaiable.
data Config = Config
    { cfg_optimze_pass_count    :: !Int
    , cfg_corelibs_path         :: FilePath
    , cfg_search_paths          :: SearchPaths
    , cfg_verbose               :: Bool
    , cfg_ddump                 :: Set.Set DDumpOpt
    }


makeConfig :: Int -> String -> FilePath -> [DDumpOpt] -> Bool -> Config
makeConfig i ss coreloc dopts verbo = Config 
    { cfg_optimze_pass_count    = i
    , cfg_corelibs_path         = coreloc
    , cfg_search_paths          = makeSearchPaths ss 
    , cfg_ddump                 = Set.fromList dopts
    , cfg_verbose               = verbo
    }



-- | Log present for both success and failure.
--
newtype Compiler a = CM { 
    getCM :: Config -> St -> IO (Log,St,Either ErrMsg a) }

instance Functor Compiler where
  fmap f ma = CM $ \r s -> getCM ma r s >>= \(w1,s1,ans) -> 
                           return (w1,s1, fmap f ans)

instance Applicative Compiler where
  pure a    = CM $ \_ s -> return (emptyH, s, Right a)
  af <*> av = CM $ \r s -> 
      getCM af r s >>= \(w1,s1,ef) -> case ef of 
          Left err -> return (w1, s1, Left err)
          Right f -> getCM av r s1 >>= \(w2,s2,ea) ->
                     return (w1 `appendH` w2, s2, fmap f ea)

instance Monad Compiler where
  return   = pure
  ma >>= k = CM $ \r s -> 
      getCM ma r s >>= \(w1,s1,ea) -> case ea of
          Left err -> return (w1, s1, Left err)
          Right a -> getCM (k a) r s1 >>= \(w2,s2,eb) ->
                     return (w1 `appendH` w2, s2, eb)



runCompilerMonad :: Config -> Compiler a -> IO (String, Either ErrMsg a)
runCompilerMonad cfg ma = 
    getCM ma cfg state_zero >>= \(w,_,a) -> return (logText w,a)


liftIO :: IO a -> Compiler a
liftIO ma = CM $ \_ s -> ma >>= \ans -> return (emptyH, s, Right ans) 


initProductiveCount :: Compiler ()
initProductiveCount = CM $ \_ s ->
    let s1 = s { productive_count = 0 }
    in return (emptyH, s1, Right ())

incrProductiveCount :: Compiler ()
incrProductiveCount = CM $ \_ s -> 
    let i = productive_count s 
    in return (emptyH, s { productive_count  = i + 1} , Right ())

getProductiveCount :: Compiler Int
getProductiveCount = CM $ \_ s -> return (emptyH, s, Right $ productive_count s)



initNameGen :: Compiler ()
initNameGen = CM $ \_ s -> 
    let s1 = s { ivar_count = 1, kvar_count = 1, avar_count = 1, lam_count = 1 }
    in return (emptyH, s1, Right ())


getVarCount :: RateId -> St -> Int
getVarCount I_RATE = ivar_count
getVarCount K_RATE = kvar_count
getVarCount A_RATE = avar_count



incrCount :: RateId -> St -> St
incrCount I_RATE = (\s i -> s { ivar_count = i+1 }) <*> ivar_count
incrCount K_RATE = (\s i -> s { kvar_count = i+1 }) <*> kvar_count
incrCount A_RATE = (\s i -> s { avar_count = i+1 }) <*> avar_count 


fresh :: RateId -> Compiler VarId
fresh ri = CM $ \_ s -> 
    let i = getVarCount ri s
    in return (emptyH, incrCount ri s, Right $ Local ri i)

freshLamId :: Compiler VarId
freshLamId = CM $ \_ s -> 
    let i = lam_count s
    in return (emptyH, s { lam_count = i + 1 } , Right $ LamName i)



-- | Derive a fresh variable from another.
--
refresh :: VarId -> Compiler VarId
refresh (Local ri _)    = fresh ri
refresh (LamName {})    = freshLamId
refresh (Final ri s)    = return $ Final ri s



asksCfg :: (Config -> a) -> Compiler a
asksCfg fn = CM $ \r s -> return (emptyH, s, Right $ fn r)

askPassCount :: Compiler Int
askPassCount = asksCfg cfg_optimze_pass_count


askBuiltinNames :: Compiler [Ident]
askBuiltinNames = return []

{-
askBuiltinNames = asksCfg (fn . cfg_builtins)
  where
    fn = Map.keys
-}

askSearchPaths :: Compiler [FilePath]
askSearchPaths = getSearchPaths <$> asksCfg cfg_search_paths


queryDDumpOpt :: DDumpOpt -> Compiler Bool
queryDDumpOpt opt = 
    (||) <$> asksCfg cfg_verbose <*> fmap (Set.member opt) (asksCfg cfg_ddump)


local :: (Config -> Config) -> Compiler a -> Compiler a
local f ma = CM $ \r s -> getCM ma (f r) s



throwError :: String -> String -> Compiler a
throwError loc err = CM $ \_ s -> return (emptyH, s, Left $ render doc)
  where
    doc =  text loc <> char ':' <+> text "ERROR:"
       $+$ text "    " <> text err


tellDoc :: Doc -> Compiler ()
tellDoc d = CM $ \_ s -> return (wrapH d, s, Right ())


-- Warnings, GHC prints:
--
-- > File\src-loc: WARNING: 
-- >     Explanatory text message...
--
-- However we don\'t have a strong notion of source locations

warning :: String -> String -> Compiler ()
warning loc body = tellDoc $ 
        text loc <> char ':' <+> text "WARNING:"
    $+$ text "    " <> text body



ddump :: String -> Doc -> Compiler ()
ddump title body = tellDoc $ 
    vspaced [ mark <+> text title <+> mark
            , body
            , blank
            ]
  where
    mark = text "===================="