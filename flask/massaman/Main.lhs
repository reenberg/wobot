\section{Driver}

\begin{code}
-- Copyright (c) 2006-2008
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
-- Module      :  Main
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Main where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import System.Environment (getArgs)
import System.IO (hPutStrLn,
                  stderr)

import Compiler
import Compiler.Opt
import Driver (DriveM,
               emptyDriveState,
               evalDriveM,
               evalFile,
               compileFile,
               userInteract)
\end{code}

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    (opts, files) <- parseOpts defaultOpts args
    when (isFlagSet Opt_eval opts || isFlagSet Opt_interactive opts) $ do
        intEnv <- emptyDriveState opts
        result <- evalDriveM (eval opts files) intEnv
        case result of
          Left err  -> liftIO $ hPutStrLn stderr $ show err
          Right _   -> return ()
    when (isFlagSet Opt_check opts || isFlagSet Opt_compile opts) $
         mapM_ (compile opts) files
  where
    defaultOpts :: Opts
    defaultOpts = Opts  {  output    = Nothing
                        ,  prelude   = Nothing
                        ,  flags     = [Opt_RecursiveTypes,
                                        Opt_RecursiveFunctions,
                                        Opt_Closures,
                                        Opt_ImplicitPrelude]
                        }
\end{code}

\begin{code}
eval :: Opts -> [String] -> DriveM ()
eval opts files = do
    forPrelude () evalFile
    mapM_ evalFile files
    when (isFlagSet Opt_interactive opts)
        userInteract
\end{code}

\begin{code}
compile :: Opts -> String -> IO ()
compile opts filename = do
    intEnv <- emptyDriveState opts
    result <- evalDriveM (compileFile filename) intEnv
    case result of
      Left err  -> liftIO $ hPutStrLn stderr $ show err
      Right _   -> return ()
\end{code}
