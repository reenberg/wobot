%if False
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

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
-- Module      :  Check.F.Exceptions
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.F.Exceptions (
    SomeTcException(..),
    Duplicates(..),
    UnsaturatedConstructor(..),
    IllegalConstructorDeclaration(..),
    UnboundConstructor(..),
    UnboundTypeConstructor(..),
    UnboundTypeVariable(..),
    UnboundTypeFunction(..),
    UnboundVariable(..),
    SortMatchError(..),
    KindMatchError(..),
    TypeMatchError(..),
    NotSortTY(..),
    NotSortCO(..),
    NotForAll(..),
    NotAppTy(..)
  ) where

import Control.Exception
import Data.Typeable (Typeable,
                      cast)

import Data.Loc
import Data.Loc.Pretty ()
import Data.Name
import Language.F
import Text.PrettyPrint.Mainland
\end{code}
%endif

\subsection{Type Checking Exceptions}

\begin{code}
data SomeTcException = forall a . (Pretty a, Exception a) => SomeTcException a
  deriving (Typeable)

instance Show SomeTcException where
    show (SomeTcException e) = show e

instance Pretty SomeTcException where
    ppr (SomeTcException e) = ppr e

instance Exception SomeTcException

tcToException  ::  (Pretty a, Exception a)
               =>  a -> SomeException
tcToException = toException . SomeTcException

tcFromException  ::  (Pretty a, Exception a)
                 =>  SomeException -> Maybe a
tcFromException x = do
    SomeTcException a <- fromException x
    cast a
\end{code}

\begin{code}
data Duplicates = Duplicates Doc [[(Name, Loc)]]
  deriving (Typeable)

instance Exception Duplicates where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty Duplicates where
    ppr (Duplicates desc ns) =
        nest 4 $ stack (map pprGroup ns)
      where
        pprGroup :: [(Name, Loc)] -> Doc
        pprGroup ns = nest 4 $
            desc <+> squotes (ppr $ fst $ head ns) <+> text "at:" </>
            stack (map (ppr . snd) ns)
\end{code}

\begin{code}
data UnsaturatedConstructor = UnsaturatedConstructor Con
  deriving (Typeable)

instance Exception UnsaturatedConstructor where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnsaturatedConstructor where
    ppr (UnsaturatedConstructor con) =
        text "Unsaturated constructor:" <+> ppr con
\end{code}

\begin{code}
data IllegalConstructorDeclaration = IllegalConstructorDeclaration Con
  deriving (Typeable)

instance Exception IllegalConstructorDeclaration where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty IllegalConstructorDeclaration where
    ppr (IllegalConstructorDeclaration con) =
        text "Illegal constructor declaration:" <+> ppr con
\end{code}

\begin{code}
data UnboundConstructor = UnboundConstructor Con
  deriving (Typeable)

instance Exception UnboundConstructor where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundConstructor where
    ppr (UnboundConstructor con) =
        text "Constructor" <+> squotes (ppr con) <+> text "not in scope"
\end{code}

\begin{code}
data UnboundTypeConstructor = UnboundTypeConstructor TyCon
  deriving (Typeable)

instance Exception UnboundTypeConstructor where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundTypeConstructor where
    ppr (UnboundTypeConstructor tycon) =
        text "Type constructor" <+> squotes (ppr tycon) <+> text "not in scope"
\end{code}

\begin{code}
data UnboundTypeVariable = UnboundTypeVariable TyVar
  deriving (Typeable)

instance Exception UnboundTypeVariable where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundTypeVariable where
    ppr (UnboundTypeVariable tyvar) =
        text "Type variable" <+> squotes (ppr tyvar) <+> text "not in scope"
\end{code}

\begin{code}
data UnboundTypeFunction = UnboundTypeFunction TyFun
  deriving (Typeable)

instance Exception UnboundTypeFunction where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundTypeFunction where
    ppr (UnboundTypeFunction tyfun) =
        text "Type function" <+> squotes (ppr tyfun) <+> text "not in scope"
\end{code}

\begin{code}
data UnboundVariable = UnboundVariable Var
  deriving (Typeable)

instance Exception UnboundVariable where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundVariable where
    ppr (UnboundVariable v) =
        text "Variable" <+> squotes (ppr v) <+> text "not in scope"
\end{code}

\begin{code}
data SortMatchError = SortMatchError Sort Sort
  deriving (Typeable)

instance Exception SortMatchError where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty SortMatchError where
    ppr (SortMatchError sigma1 sigma2) =
        text "Expected sort:" <+> ppr sigma2 </>
        text "     Inferred:" <+> ppr sigma1
\end{code}

\begin{code}
data KindMatchError = KindMatchError Kind Kind
  deriving (Typeable)

instance Exception KindMatchError where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty KindMatchError where
    ppr (KindMatchError kappa1 kappa2) =
        text "Expected kind:" <+> ppr kappa2 </>
        text "     Inferred:" <+> ppr kappa1
\end{code}

\begin{code}
data TypeMatchError = TypeMatchError Type Type
  deriving (Typeable)

instance Exception TypeMatchError where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty TypeMatchError where
    ppr (TypeMatchError tau1 tau2) =
        text "Expected type:" <+> ppr tau2 </>
        text "     Inferred:" <+> ppr tau1
\end{code}

\begin{code}
data NotSortTY = NotSortTY Type
  deriving (Typeable)

instance Exception NotSortTY where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty NotSortTY where
    ppr (NotSortTY tau) =
        text "Type not of sort TY:" <+> ppr tau
\end{code}

\begin{code}
data NotSortCO = NotSortCO Type
  deriving (Typeable)

instance Exception NotSortCO where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty NotSortCO where
    ppr (NotSortCO tau) =
        text "Type not of sort CO:" <+> ppr tau
\end{code}

\begin{code}
data NotForAll = NotForAll Type
  deriving (Typeable)

instance Exception NotForAll where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty NotForAll where
    ppr (NotForAll tau) =
        text "Expected forall type but got:" <+> ppr tau
\end{code}

\begin{code}
data NotAppTy = NotAppTy Type
  deriving (Typeable)

instance Exception NotAppTy where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty NotAppTy where
    ppr (NotAppTy tau) =
        text "Expected type application but got:" <+> ppr tau
\end{code}
