{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Compile
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
--
--
--------------------------------------------------------------------------------


module OboeLang.Compile
  ( 

    CompilerOpts(..)
  , compile
  , runCompile


  ) where

import OboeLang.Alpha
import OboeLang.Assoc
import OboeLang.Beta
import OboeLang.CompilerMon
import OboeLang.ConstFold
import qualified OboeLang.CsoundSyntax          as Csound
import qualified OboeLang.CsoundTrans           as Csound
import OboeLang.ElimLet
import qualified OboeLang.ElimTuple             as Inter
import qualified OboeLang.FormSyntax            as Forms
import qualified OboeLang.FormTrans             as Forms
import qualified OboeLang.Inline                as Inter
import qualified OboeLang.InlineLetfun          as Inter
import OboeLang.ModuleIO
import qualified OboeLang.Syntax                as Inter
import qualified OboeLang.ToplevelTrans         as Top
import qualified OboeLang.TopSyntax             as Top
import OboeLang.Utils.Common ( repeatM, debugM ) 
import OboeLang.Utils.Pretty


import Text.PrettyPrint.HughesPJ (render )      -- package: pretty

import Control.Applicative
import Control.Monad
import System.Exit
import System.FilePath
import Prelude hiding ( log )



data CompilerOpts = CompilerOpts
    { opt_search_path           :: SearchPath
    , opt_builtins_loc          :: FilePath
    , opt_optimize_count        :: Int
    , opt_verbose               :: Bool
    , opt_ddump_flags           :: [DDumpOpt]
    , opt_output_file           :: FilePath
    }


compile :: CompilerOpts -> FilePath -> IO ()
compile = flip runCompile 


-- Potentially should support something like GHC\'s -ddump-pass

runCompile :: FilePath -> CompilerOpts -> IO ()
runCompile main_path (CompilerOpts { opt_search_path    = searches
                                   , opt_builtins_loc   = builtins_path
                                   , opt_optimize_count = pc
                                   , opt_verbose        = verbo
                                   , opt_ddump_flags    = dd_flags 
                                   , opt_output_file    = out1 }) = 
    do { (log,ans) <- runCompilerMonad config $ 
                        do { (builts,prgm) <- loadProgram builtins_path main_path
                           ; withBuiltins builts (compileProgram prgm)
                           }
       ; case ans of
           Left err -> putStrLn log >> exitWithErr err
           Right a -> putStrLn log >> writeFile outpath (render $ pretty a)
       }
  where
    config = makeConfig pc searches dd_flags verbo
    exitWithErr err = do { putStrLn err; exitWith $ ExitFailure (-1) }
    outpath = if null out1 then (replaceExtension main_path "orc") else out1


-- | Ideally this function should do each part of the compilation 
-- step-by-step and on the /most obvious/ objects. Then this 
-- would be the only function that we need to annotate with ddump- 
-- calls
-- 
compileProgram :: Top.Program -> Compiler Csound.Program
compileProgram prgm =   
    debugM return               ddumpParsed prgm        >>= 
    debugM stepTransToForms     ddumpTransToForms       >>= 
    debugM stepTransToInter     ddumpTransToInter       >>=
    debugM stepInline           ddumpInline             >>=
    debugM stepInlineLetfun     ddumpInlineLetfun       >>=
    debugM stepElimTuple        ddumpElimTuple          >>=
    debugM stepOptimizeInter    ddumpOptimizeInter      >>=
    stepTransToCsound
    



stepTransToForms :: Top.Program -> Compiler Forms.Program
stepTransToForms = Top.translate

stepTransToInter :: Forms.Program -> Compiler Inter.Program
stepTransToInter = Forms.translate

stepInline :: Inter.Program -> Compiler Inter.Program
stepInline = Inter.inline

         

stepInlineLetfun :: Inter.Program -> Compiler Inter.Program
stepInlineLetfun = Inter.inlineLetfun




stepElimTuple :: Inter.Program -> Compiler Inter.Program
stepElimTuple = Inter.elimTuple

stepOptimizeInter :: Inter.Program -> Compiler Inter.Program
stepOptimizeInter = Inter.mapM_Instrument optimizeInst


stepTransToCsound :: Inter.Program -> Compiler Csound.Program
stepTransToCsound = Csound.translate




-- | Finish with an alpha rename so we get variables starting 
-- from 1.
--
optimizeInst  :: Inter.Instrument -> Compiler Inter.Instrument
optimizeInst ins@(Inter.Instrument {Inter.instr_body = emain}) = 
    (\main1 -> ins {  Inter.instr_body = main1 }) <$> process emain
  where
    process = alphaExpr >=> (\a -> askPassCount >>= \i -> repeatM i optimizeSteps a) 
                        >=> alphaExpr



-- | In MinCaml the optimization steps are repeated as a round
-- of simplification may introduce further opportunities for
-- optimization.
--
optimizeSteps :: Inter.Expr -> Compiler Inter.Expr
optimizeSteps = elimLet <=< constFold <=< assoc <=< beta
   





----------------------------------------------------------------------------------
-- Debugging dump

ddumpParsed :: Top.Program -> Compiler ()
ddumpParsed prgm = queryDDumpOpt DDUMP_PARSED >>= \a -> 
    when a $ ddump "Parsed" (pretty prgm)


ddumpTransToForms :: Forms.Program -> Compiler ()
ddumpTransToForms prgm = queryDDumpOpt DDUMP_FORMS >>= \a -> 
    when a $ ddump "Forms" (pretty prgm)

ddumpTransToInter :: Inter.Program -> Compiler ()
ddumpTransToInter prgm = queryDDumpOpt DDUMP_INTER >>= \a -> 
    when a $ ddump "Intermediate" (pretty prgm)

ddumpInline :: Inter.Program -> Compiler ()
ddumpInline prgm = queryDDumpOpt DDUMP_INLINE >>= \a -> 
    when a $ ddump "Inlined" (pretty prgm)


ddumpInlineLetfun :: Inter.Program -> Compiler ()
ddumpInlineLetfun orch = queryDDumpOpt DDUMP_INLINE_LETFUN >>= \a ->
    when a $ ddump "Inline Letfuns"  (pretty orch)


ddumpElimTuple :: Inter.Program -> Compiler ()
ddumpElimTuple orch = queryDDumpOpt DDUMP_ELIM_TUPLE >>= \a ->
    when a $ ddump "Eliminate Tuples"  (pretty orch)


ddumpOptimizeInter :: Inter.Program -> Compiler ()
ddumpOptimizeInter orch = queryDDumpOpt DDUMP_OPTIMIZE_INTER >>= \a ->
    when a $ ddump "Optimize"  (pretty orch)

