{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.ModuleIO
-- Copyright   :  (c) Stephen Tetley 2015
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

import OboeLang.CompilerMon
import OboeLang.Parser
import OboeLang.SyntaxCommon
import OboeLang.TopSyntax

import Text.Parsec.Error                -- package: parsec

import Data.Foldable ( foldrM )
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import Text.Printf

loadProgram :: FilePath -> Compiler (Program)
loadProgram main_path = do 
    { mod_main          <- loadRootModule main_path
    ; libs              <- return []
    ; return $ Program { prog_main = mod_main
                       , prog_extra_mods = libs 
                       }
    }


liftParse :: IO (Either ParseError a) -> Compiler a
liftParse parse_ans = liftIO parse_ans >>= next
  where
    next (Left err) = throwError "ModuleIO.liftParse" (show err)
    next (Right a)  = return a


loadRootModule :: FilePath -> Compiler Module
loadRootModule = liftParse . parseRootModule


findUserModule :: ModuleName -> Compiler FilePath
findUserModule mod_name = askSearchPaths >>= step 
  where
    errK        = throwError "ModuleIO.findUserModule"
                             (printf "Cannot find module: %s" 
                                     (moduleNameString mod_name))

    step []     = process1 "" errK
    step (x:xs) = process1 x (step xs)

    process1 ss end = let file_name = moduleFilePath ss mod_name in
                      do { ans <- liftIO $ doesFileExist file_name
                         ; if ans then return file_name else end }



findCoreModule :: ModuleName -> Compiler FilePath
findCoreModule mod_name = do 
    { coreroot          <- askCoreLibsPath
    ; let file_name     = moduleFilePath coreroot mod_name
    ; ans               <- liftIO $ doesFileExist file_name
    ; if ans then return file_name else errK 
    }
  where
    errK = throwError "ModuleIO.findCoreModule"
                      (printf "Cannot find module: %s" 
                              (moduleNameString mod_name))


{-    
-- | Module names should be the same as file names except for 
-- Main.
--
-- The Main file can have any name but the module name should 
-- be Main.
--
buildFullName :: FilePath -> ModuleName -> FilePath
buildFullName prefix mod_name = 
    normalise $ prefix `combine` (makeSuffix $ deconsModuleName mod_name)
  where
    makeSuffix (xs,x)    = let front = joinPath xs; back = x <.> "oboe" 
                           in front `combine` back

-}

{-



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

-}