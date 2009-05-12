\section{Command-line Options}

%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

-- Copyright (c) 2007-2008
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Compiler.Opt
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Compiler.Opt (
    Opts(..),
    Flag(..),
    isFlagSet,
    MonadOpts(..),
    parseOpts
  ) where

import Control.Monad (foldM)
import Data.List (delete,
                  isPrefixOf)
import System.Console.GetOpt
\end{code}
%endif

\begin{code}
data Opts = Opts  {  output   :: Maybe String
                  ,  prelude  :: Maybe String
                  ,  platform :: Maybe String
                  ,  flags    :: [Flag]
                  }
  deriving(Show)
\end{code}

\begin{code}
data Flag  =  Opt_verbose
           |  Opt_check
           |  Opt_eval
           |  Opt_interactive
           |  Opt_compile
           |  Opt_d_dump_hs
           |  Opt_d_dump_f
           |  Opt_d_dump_simpl
           |  Opt_d_dump_c
           |  Opt_d_check_f
           |  Opt_d_dump_tc_trace
           |  Opt_d_dump_ftc_trace
           |  Opt_d_dump_match_trace
           |  Opt_d_dump_class_trace
           |  Opt_d_dump_simpl_trace
           |  Opt_d_dump_eval_trace
           |  Opt_d_dump_toc_trace
           |  Opt_RecursiveTypes
           |  Opt_RecursiveFunctions
           |  Opt_Closures
           |  Opt_Strict
           |  Opt_OptC
           |  Opt_ImplicitPrelude
           |  Opt_EmptyDataDecls
           |  Opt_LiberalTypeSynonyms
           |  Opt_MultiParamTypeClasses
           |  Opt_FlexibleContexts
           |  Opt_FlexibleInstances
           |  Opt_ConstrainedClassMethods
           |  Opt_FunctionalDependencies
           |  Opt_OverlappingInstances
           |  Opt_IncoherentInstances
           |  Opt_simpl
  deriving (Eq, Show)

setFlag :: Flag -> Opts -> IO Opts
setFlag f opts
    | f `elem` flags opts  = return opts
    | otherwise            = return $ opts { flags = f : flags opts }

clearFlag :: Flag -> Opts -> IO Opts
clearFlag f opts = return $ opts { flags = delete f (flags opts) }

isFlagSet :: Flag -> Opts -> Bool
isFlagSet f opts = f `elem` flags opts
\end{code}

\begin{code}
xFlags :: [(String, Flag)]
xFlags = [
  ("RecursiveTypes",           Opt_RecursiveTypes),
  ("RecursiveFunctions",       Opt_RecursiveFunctions),
  ("Closures",                 Opt_Closures),
  ("Strict",                   Opt_Strict),
  ("OptC",                     Opt_OptC),
  ("ImplicitPrelude",          Opt_ImplicitPrelude),
  ("EmptyDataDecls",           Opt_EmptyDataDecls),
  ("LiberalTypeSynonyms",      Opt_LiberalTypeSynonyms),
  ("MultiParamTypeClasses",    Opt_MultiParamTypeClasses),
  ("FlexibleContexts",         Opt_FlexibleContexts),
  ("FlexibleInstances",        Opt_FlexibleInstances),
  ("ConstrainedClassMethods",  Opt_ConstrainedClassMethods),
  ("FunctionalDependencies",   Opt_FunctionalDependencies),
  ("OverlappingInstances",     Opt_OverlappingInstances),
  ("IncoherentInstances",      Opt_IncoherentInstances)
  ]

handleXFlag :: String -> Opts -> IO Opts
handleXFlag f opts
    |"No" `isPrefixOf` f  = clearXFlag (drop 2 f) opts
    | otherwise           = setXFlag f opts

setXFlag :: String -> Opts -> IO Opts
setXFlag xf opts =
    case lookup xf xFlags of
      Just f  -> setFlag f opts
      Nothing -> fail $ "no language option " ++ xf

clearXFlag :: String -> Opts -> IO Opts
clearXFlag xf opts =
    case lookup xf xFlags of
      Just f  -> clearFlag f opts
      Nothing -> fail $ "no language option " ++ xf
\end{code}

\begin{code}
fFlags :: [(String, Flag)]
fFlags = [
  ("simpl",  Opt_simpl)
  ]

handleFFlag :: String -> Opts -> IO Opts
handleFFlag f opts
    | "no-" `isPrefixOf` f  = clearFFlag (drop 3 f) opts
    | "no" `isPrefixOf` f   = clearFFlag (drop 2 f) opts
    | otherwise             = setFFlag f opts

setFFlag :: String -> Opts -> IO Opts
setFFlag ff opts =
    case lookup ff fFlags of
      Just f  -> setFlag f opts
      Nothing -> fail $ "no compiler flag " ++ ff

clearFFlag :: String -> Opts -> IO Opts
clearFFlag ff opts =
    case lookup ff fFlags of
      Just f  -> clearFlag f opts
      Nothing -> fail $ "no compiler flag " ++ ff
\end{code}

\begin{code}
class Monad m => MonadOpts m where
    optVal   :: (Opts -> a) -> m a
    getOpts  :: m Opts
    setOpts  :: Opts -> m ()
\end{code}

\begin{code}
options :: [OptDescr (Opts -> IO Opts)]
options =
    [ Option  ['v']  ["verbose"]            (NoArg (setFlag Opt_verbose))
                                            "verbose",

      Option  []     ["check"]              (NoArg (setFlag Opt_check))
                                            "check",

      Option  ['e']  ["eval"]               (NoArg (setFlag Opt_eval))
                                            "evaluate",

      Option  ['i']  ["interactive"]        (NoArg (setFlag Opt_interactive))
                                            "run interactively",

      Option  ['c']  []                     (NoArg (setFlag Opt_compile))
                                            "compile",

      Option  ['o']  ["output"]             (ReqArg (\f o -> return $ o { output=Just f })
                                                        "FILE")
                                            "output",

      Option  []     ["prelude"]            (ReqArg (\f o -> return $ o { prelude=Just f })
                                                        "FILE")
                                            "prelude",

      Option  []     ["ddump-hs"]           (NoArg (setFlag Opt_d_dump_hs))
                                            "dump Hs output",

      Option  []     ["ddump-f"]            (NoArg (setFlag Opt_d_dump_f))
                                            "dump F output",

      Option  []     ["ddump-simpl"]        (NoArg (setFlag Opt_d_dump_simpl))
                                            "dump F output of simplify stage",

      Option  []     ["ddump-c"]            (NoArg (setFlag Opt_d_dump_c))
                                            "dump C output",

      Option  []     ["dcheck-f"]           (NoArg (setFlag Opt_d_check_f))
                                            "always type check F",

      Option  []     ["ddump-match-trace"]  (NoArg (setFlag Opt_d_dump_match_trace))
                                            "trace the match compiler",

      Option  []     ["ddump-tc-trace"]     (NoArg (setFlag Opt_d_dump_tc_trace))
                                            "trace the type checker",

      Option  []     ["ddump-ftc-trace"]    (NoArg (setFlag Opt_d_dump_ftc_trace))
                                            "trace the F type checker",

      Option  []     ["ddump-class-trace"]  (NoArg (setFlag Opt_d_dump_class_trace))
                                            "trace type classes",

      Option  []     ["ddump-simpl-trace"]  (NoArg (setFlag Opt_d_dump_simpl_trace))
                                            "trace simplification",

      Option  []     ["ddump-eval-trace"]   (NoArg (setFlag Opt_d_dump_eval_trace))
                                            "trace the evaluator",

      Option  []     ["ddump-toc-trace"]    (NoArg (setFlag Opt_d_dump_toc_trace))
                                            "trace the C translator",

      Option  ['f']  []                     (ReqArg handleFFlag "FEATURE")
                                            "compiler flag",

      Option  ['X']  []                     (ReqArg handleXFlag "FEATURE")
                                            "language feature",

      Option  []     ["platform"]            (ReqArg (\f o -> return $ o { platform=Just f })
                                                        "FILE")
                                            "prelude"
    ]
\end{code}

\begin{code}
parseOpts :: Opts -> [String] -> IO (Opts, [String])
parseOpts defaults argv =
    case getOpt Permute options argv of
      (mfs,  n,  []  )  -> do  opts <- foldM (\o f -> f o) defaults mfs
                               return (opts, n)
      (_,  _,  errs)    -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: parse [OPTION...] files..."
\end{code}
