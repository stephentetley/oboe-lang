{-# OPTIONS -Wall #-}

module Demo01 where

import Main
import OboeLang.CompilerMon ( DDumpOpt(..) )

import OboeLang.Parser
import OboeLang.TopSyntax
import OboeLang.Utils.Pretty

import System.Environment

opts :: Options
opts = default_options { opt_corelibs_loc = "../core_lib/csound"
                       , opt_search_path  = "test"
                       , opt_ddump_flags  = [DDUMP_PARSED, DDUMP_FORMS, DDUMP_INLINE]
                       , opt_verbose      = False }


demo01  = main2 opts ["test/Instr1.oboe"]


demo02 = do 
    ans <- parseModule "test/Instr1.oboe"
    case ans of 
      Left err -> print err
      Right mod1 -> do { print $ pretty mod1
                       ; print mod1
                       }

-- returns ghc path when used from ghci
test01 = print =<< getExecutablePath