{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ModuleIO
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Loading Modules...
--
--------------------------------------------------------------------------------


module OboeLang.ModuleIO
  ( 
    loadProgram
  ) where

import OboeLang.Builtin
import OboeLang.CompilerMon
import OboeLang.Parser
import OboeLang.SyntaxCommon
import OboeLang.TopSyntax




import Data.Foldable ( foldrM )
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import Text.Printf

loadProgram :: FilePath -> FilePath 
            -> Compiler ([Builtin],Program)
loadProgram _ _ = error "loadProgram"

{-

loadProgram :: FilePath -> FilePath 
            -> Compiler ([Builtin],Program)
loadProgram builts_path main_path = do 
    { builts            <- loadBuiltins builts_path
    ; mod_main          <- loadMainModule main_path
    ; mod_dict          <- loadChildModules Map.empty mod_main
    ; let prgm          = Program { prog_main       = mod_main
                                  , prog_extra_libs = map snd $ Map.toAscList mod_dict
                                  }
    ; return (builts,prgm)
    }



findLibraryModule :: ModuleName -> Compiler FilePath
findLibraryModule mod_name = askSearchPaths >>= step 
  where
    errK        = throwError "ModuleIO.findLibraryModule"
                             (printf "Cannot find module: %s" 
                                     (moduleNameString mod_name))

    step []     = process1 "" errK
    step (x:xs) = process1 x (step xs)

    process1 ss end = let file_name = resolveFileName ss mod_name in
                      do { ans <- liftIO $ doesFileExist file_name
                         ; if ans then return file_name else end }


-- | Module names should be the same as file names except for 
-- Main.
--
-- The Main file can have any name but the module name should 
-- be Main.
--
resolveFileName :: FilePath -> ModuleName -> FilePath
resolveFileName prefix mod_name = 
    normalise $ prefix `combine` (makeSuffix $ deconsModuleName mod_name)
  where
    makeSuffix (xs,x)    = let front = joinPath xs; back = x <.> "ochre" 
                           in front `combine` back


liftParse :: IO (Either ParseError a) -> Compiler a
liftParse parse_ans = liftIO parse_ans >>= next
  where
    next (Left err) = throwError "ModuleIO.liftParse" (show err)
    next (Right a)  = return a

loadBuiltins :: FilePath -> Compiler [Builtin]
loadBuiltins _ = return []
-- loadBuiltins = liftParse . parseBuiltins

loadMainModule :: FilePath -> Compiler Module
loadMainModule = liftParse . parseMainModule

loadLibraryModule :: ModuleName -> Compiler Module
loadLibraryModule mod_name = findLibraryModule mod_name >>= liftParse . parseLibraryModule


loadChildModules :: ModuleDict -> Module -> Compiler ModuleDict
loadChildModules dict = foldrM fn dict . module_imports 
  where
    fn imp_decl ac = let mod_name = importName imp_decl 
                     in if Map.member mod_name ac 
                        then return ac
                        else do { a <- loadLibraryModule mod_name
                                ; let d2 = Map.insert mod_name a dict
                                ; loadChildModules d2 a
                                }

importName :: ImportDecl -> ModuleName
importName (ImportTop name)     = name
importName (ImportAlias name _) = name

-}