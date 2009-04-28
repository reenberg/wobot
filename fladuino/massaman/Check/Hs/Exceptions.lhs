\section{Type Checking Exceptions}

%if False
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Copyright (c) 2008
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

--------------------------------------------------------------------------------
-- |
-- Module      :  Check.Hs.Exceptions
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.Hs.Exceptions where

import Control.Exception
import Data.Typeable (Typeable,
                      cast)

import Data.Loc
import Data.Loc.Pretty ()
import Data.Name
import Language.Hs
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

data IllegalRecursion = IllegalRecursion Doc [(Name, Loc)]
  deriving (Typeable)

instance Exception IllegalRecursion where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty IllegalRecursion where
    ppr (IllegalRecursion desc ns) =
        nest 4 $ desc <+> stack (map pprOcc ns)
      where
        pprOcc :: (Name, Loc) -> Doc
        pprOcc (n, loc) = squotes (ppr n) <+> text "at:" <+> ppr loc

data PartiallyAppliedSynonym = PartiallyAppliedSynonym TyCon Int Int
  deriving (Typeable)

instance Exception PartiallyAppliedSynonym where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty PartiallyAppliedSynonym where
    ppr (PartiallyAppliedSynonym tycon needed given) =
        text "Type synonym" <+> squotes (ppr tycon)
        <+> text "should have" <+> text (show needed)
        <+> text "arguments but was given" <+> text (show given)

data CyclicSynonyms = CyclicSynonyms [Decl]
  deriving (Typeable)

instance Exception CyclicSynonyms where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty CyclicSynonyms where
    ppr (CyclicSynonyms typedecls) =
        nest 4 $  text "Cycle in type synonym declarations:"
                  </> stack (map ppr typedecls)

data UnboundSynonymTypeVariables = UnboundSynonymTypeVariables TyCon [TyVar]
  deriving (Typeable)

instance Exception UnboundSynonymTypeVariables where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundSynonymTypeVariables where
    ppr (UnboundSynonymTypeVariables tycon tvs) =
        nest 4 $  text "Type synonym"
                  <+> squotes (ppr tycon)
                  <+> text "uses the following type"
                  <+> text "variables without defining them:"
                  </> commasep (map ppr tvs)

data DifferingEquationArgCounts = DifferingEquationArgCounts Var
  deriving (Typeable)

instance Exception DifferingEquationArgCounts where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty DifferingEquationArgCounts where
    ppr (DifferingEquationArgCounts v) =
        text "Equations for" <+> squotes (ppr v)
        <+> text "have differing number of arguments"

data DifferingTypeArgCounts = DifferingTypeArgCounts Var
  deriving (Typeable)

instance Exception DifferingTypeArgCounts where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty DifferingTypeArgCounts where
    ppr (DifferingTypeArgCounts v) =
        text "Type signature for" <+> squotes (ppr v)
        <+> text "has differing number of arguments from its definition"

data UnboundTypeConstructor = UnboundTypeConstructor TyCon
  deriving (Typeable)

instance Exception UnboundTypeConstructor where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundTypeConstructor where
    ppr (UnboundTypeConstructor tycon) =
        text "Type constructor" <+> squotes (ppr tycon) <+> text "not in scope"

data UnboundConstructor = UnboundConstructor Con
  deriving (Typeable)

instance Exception UnboundConstructor where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundConstructor where
    ppr (UnboundConstructor con) =
        text "Constructor" <+> squotes (ppr con) <+> text "not in scope"

data UnboundTypeVariable = UnboundTypeVariable TyVar
  deriving (Typeable)

instance Exception UnboundTypeVariable where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundTypeVariable where
    ppr (UnboundTypeVariable tyvar) =
        text "Type variable" <+> squotes (ppr tyvar) <+> text "not in scope"

data UnboundVariable = UnboundVariable Var
  deriving (Typeable)

instance Exception UnboundVariable where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty UnboundVariable where
    ppr (UnboundVariable var) =
        text "Variable" <+> squotes (ppr var) <+> text "not in scope"

data NotPolymorphicEnough = NotPolymorphicEnough Doc Doc
  deriving (Typeable)

instance Exception NotPolymorphicEnough where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty NotPolymorphicEnough where
    ppr (NotPolymorphicEnough sigma1 _) =
        text "Type not polymorphic enough:" <+> sigma1

data TypeUnificationError = TypeUnificationError Doc Doc
  deriving (Typeable)

instance Exception TypeUnificationError where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty TypeUnificationError where
    ppr (TypeUnificationError ty1 ty2) =
        text "Expected type:" <+> ty2 </>
        text "     Inferred:" <+> ty1

data TypeOccursCheckError = TypeOccursCheckError Doc Doc
  deriving (Typeable)

instance Exception TypeOccursCheckError where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty TypeOccursCheckError where
    ppr (TypeOccursCheckError tv ty) =
        text "Cannot construct the infinite type:"
        <+> tv <+> text "=" <+> ty

data KindUnificationError= KindUnificationError Doc Doc
  deriving (Typeable)

instance Exception KindUnificationError where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty KindUnificationError where
    ppr (KindUnificationError k1 k2) =
        text "Expected kind:" <+> k2 </>
        text "     Inferred:" <+> k1

data KindOccursCheckError = KindOccursCheckError Doc Doc
  deriving (Typeable)

instance Exception KindOccursCheckError where
    toException    = tcToException
    fromException  = tcFromException

instance Pretty KindOccursCheckError where
    ppr (KindOccursCheckError kv k) =
        text "Cannot construct the infinite kind:"
        <+> kv <+> text "=" <+> k
\end{code}
