{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Main and command line options processing.
--
--------------------------------------------------------------------------------


module Main where


import qualified OboeLang.Compile as OC
import OboeLang.CompilerMon ( DDumpOpt(..) )


import System.Console.GetOpt
import System.Environment ( getArgs )
import Text.Read ( readMaybe )


header :: String
header = "Usage: oboe <main_file>"

help_message :: String
help_message = unlines $  
    [ "Oboe compiler."
    ]



data Options = Options 
    { opt_show_help             :: Bool
    , opt_search_path           :: String
    , opt_optimize_count        :: Int
    , opt_builtins_loc          :: FilePath
    , opt_ddump_flags           :: [DDumpOpt]
    , opt_verbose               :: Bool
    , opt_output_file           :: FilePath
    }
  deriving (Show)
    
default_options :: Options
default_options = Options
    { opt_show_help             = False
    , opt_search_path           = ""
    , opt_optimize_count        = 5
    , opt_builtins_loc          = ""
    , opt_ddump_flags           = []
    , opt_verbose               = False
    , opt_output_file           = ""
    }

transOptions :: Options -> OC.CompilerOpts
transOptions opts = 
    OC.CompilerOpts { OC.opt_search_path     = opt_search_path opts
                    , OC.opt_builtins_loc    = opt_builtins_loc opts
                    , OC.opt_optimize_count  = opt_optimize_count opts
                    , OC.opt_verbose         = opt_verbose opts
                    , OC.opt_ddump_flags     = opt_ddump_flags opts
                    , OC.opt_output_file     = opt_output_file opts
                    }



type OptionsS = Options -> Options


options :: [OptDescr OptionsS]
options =
    [ Option ['b'] ["builtins"] 
                   (ReqArg builtinsS "BUILTINS")     "builtins"

    , Option []    ["ddump-parsed"] 
                   (NoArg (ddumpS DDUMP_PARSED))   "debug dump after parsing"

    , Option ['h'] ["help"]     
                   (NoArg helpS)                     help_message

    , Option ['i'] ["import"]   
                   (ReqArg importsS "SEARCH_PATH")   "search path"     
    
    , Option ['o'] ["output"]
                   (ReqArg outputS "OUTPUT_FILE")  "output file"

    , Option []    ["oc"]       
                   (ReqArg optimizeS "OPTIMIZE_COUNT")     "optimize count"

    , Option ['v'] ["verbose"]
                   (NoArg verboseS)   "verbose mode"
    ]


helpS               :: OptionsS
helpS o             = o { opt_show_help = True }

ddumpS              :: DDumpOpt -> OptionsS
ddumpS opt o        = let ss = opt_ddump_flags o in o { opt_ddump_flags = opt:ss }

importsS            :: String -> OptionsS
importsS s o        = o { opt_search_path = s }

builtinsS           :: String -> OptionsS
builtinsS s o       = o { opt_builtins_loc = s }


outputS             :: FilePath -> OptionsS
outputS s o         = o { opt_output_file = s }

optimizeS           :: String -> OptionsS
optimizeS s o       = case readMaybe s of 
    Just (i :: Int) -> o { opt_optimize_count = i }
    _               -> o              


verboseS            :: OptionsS
verboseS o          = o { opt_verbose = True }



compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv = case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) default_options o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))


main :: IO ()
main = do { args <- getArgs
          ; (opts, nonopts) <- compilerOpts args
          ; main2 opts nonopts
          }

main2 :: Options -> [String] -> IO ()
main2 opts _           | opt_show_help opts = print $ usageInfo header options
main2 opts [main_path] = OC.compile (transOptions opts) main_path
main2 _    []          = ioError $ userError "No main file"
main2 _    _           = ioError $ userError "Too many main files"