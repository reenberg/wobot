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

import Distribution.Simple
import Distribution.Simple.PreProcess
import System.FilePath
import System.Posix.Files

main = defaultMainWithHooks $
       defaultHooks { hookedPreProcessors = pphooks }
  where
    defaultHooks = simpleUserHooks

    pphooks =   ("lx", ppUnlitAlex)
              : ("ly", ppUnlitHappy)
              : hookedPreProcessors defaultHooks
    ppUnlitAlex bi lbi  = chainPP ".x" ppUnlit (ppAlex bi lbi)
    ppUnlitHappy bi lbi = chainPP ".y" ppUnlit (ppHappy bi lbi)

chainPP :: String -> PreProcessor -> PreProcessor -> PreProcessor
chainPP ext ppa ppb =
    PreProcessor { platformIndependent = platformIndependent ppa &&
                                         platformIndependent ppb
                 , runPreProcessor = runPP
                 }
    where
      runPP (inBaseDir, inRelativeFile)
            (outBaseDir, outRelativeFile) verbosity = do
          runPreProcessor ppa (inBaseDir, inRelativeFile)
                              (inBaseDir, tempRelativeFile) verbosity
          runPreProcessor ppb (inBaseDir, tempRelativeFile)
                              (outBaseDir, outRelativeFile) verbosity
          removeLink $ joinPath [inBaseDir, tempRelativeFile]
        where
          tempRelativeFile = inRelativeFile ++ "." ++ ext
