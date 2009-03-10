%if False
\begin{code}
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
-- Module      :  Match
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Match (
    NPat(..),
    Equation(..),
    headPat,
    headPatTy,
    getTy,
    isVar,
    isLit,
    isCon,
    getCon,
    getConTys,
    renameUs
  ) where

import Maybe (mapMaybe)

import Check.Hs.Monad ()
import Check.Hs.Types
import Control.Monad.Exception
import qualified Language.F as F
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{Nested Patterns}

\begin{code}
data NPat r  =  WildNPat
             |  LitNPat F.Lit
             |  ConNPat F.Con [(NPat r, Rho r)]
             |  AsNPat F.Var (Rho r) (NPat r)
    deriving (Eq, Ord)
\end{code}

\subsection{Pattern Equations}

\begin{code}
data Equation r m = Equation [(NPat r, Type r)] (Type r) (F.Exp -> m F.Exp)
\end{code}

\begin{code}
instance Pretty (Equation r m) where
    ppr (Equation ms ty _) = spread (map pprMatch ms) <+> text "at" <+> ppr ty
      where
        pprMatch :: (NPat r, Type r) -> Doc
        pprMatch (p, ty) = ppr p <+> text "::" <+/> ppr ty
\end{code}

\begin{code}
headPat :: Equation r m -> NPat r
headPat (Equation  ((AsNPat _  _ p, _)  : _)  _ _)  = p
headPat (Equation  ((p, _)              : _)  _ _)  = p
headPat (Equation  []                         _ _)  =
    internalErr $ text "headPat: equation has too few patterns"
\end{code}

\begin{code}
headPatTy :: Equation r m -> Type r
headPatTy (Equation  ((_, ty) : _)  _ _)  = ty
headPatTy (Equation  []             _ _)  =
    internalErr $ text "headPatTy: equation has too few patterns"
\end{code}

\begin{code}
getTy :: Equation r m -> Type r
getTy (Equation _ ty _) = ty
\end{code}

\begin{code}
isVar :: Equation r m -> Bool
isVar q =  case headPat q of
             WildNPat  -> True
             _         -> False
\end{code}

\begin{code}
isLit :: Equation r m -> Bool
isLit q =  case headPat q of
             LitNPat _  -> True
             _          -> False
\end{code}

\begin{code}
isCon :: Equation r m -> Bool
isCon q =  case headPat q of
             (ConNPat _ _)  -> True
             _              -> False
\end{code}

\begin{code}
getCon :: Equation r m -> F.Con
getCon q =  case headPat q of
              (ConNPat con _)  ->  con
              _                ->  internalErr $ text "getCon \
                                   \called on non-constructor equation"
\end{code}

\begin{code}
getConTys :: Equation r m -> [Type r]
getConTys q =  case headPat q of
                 (ConNPat _ binds)  ->  map snd binds
                 _                  ->  internalErr $ text "getConTys \
                                        \called on non-constructor equation"
\end{code}

When translating matches, we would like to generate code that uses the original
match variable names if the original names are simple variable
patterns. However, all the code that deals with matches generates fresh variable
names for the values being matched against---it is easier to revert back to the
original variable names in one place than special case every bit of match
generation code. We fix our match variables as follows: for each match variable,
we look at all the equations and pick the first one that is a simple variable
match, using the simple variable's name as the new match variable name. If no
equation is a simple variable match, we keep using the original ``fresh'' match
variable name.

\begin{code}
renameUs :: [F.Var] -> [Equation r m] -> ([F.Var], [Equation r m])
renameUs us qs =
    let  (ms, tys, conts)  = unzip3 $ map (\(Equation ps ty cont) -> (ps, ty, cont)) qs
         (us', ms')        = rename us ms
         qs'               = map  (\(ps, ty, cont) -> Equation ps ty cont)
                                  (zip3 ms' tys conts)
    in
      (us', qs')
  where
    rename :: [F.Var] -> [[(NPat r, Type r)]] -> ([F.Var], [[(NPat r, Type r)]])
    rename  []        ms  = ([], ms)
    rename  (u : us)  ms  =
        let  m'          =  map head ms
             u'          =  case mapMaybe getPatVar m' of
                              (v : _)  -> v
                              _        -> u
             (us', ms')  =  rename us (map tail ms)
        in
          (u' : us', map (uncurry (:)) (m' `zip` ms'))

    getPatVar :: (NPat r, Type r) -> Maybe F.Var
    getPatVar (AsNPat v _ WildNPat, _)  = Just v
    getPatVar _                         = Nothing
\end{code}

\begin{code}
pprConPatArg :: (NPat r, Rho r) -> Doc
pprConPatArg  p@(AsNPat _ _ WildNPat, _)  = pprPrec 11 p
pprConPatArg  (p, ty)                     = ppr p <+> text "::" <+/> ppr ty

instance Pretty (NPat r) where
    pprPrec  _  WildNPat           = text "_"
    pprPrec  _  (LitNPat lit)      = ppr lit

    pprPrec  p  (ConNPat (F.TupleCon _) pats) =
        parensIf (p >= 11) $
        parens (commasep (map pprConPatArg pats))

    pprPrec  p  (ConNPat con binds) =
        parensIf (p >= 11) $
        (spread $ ppr con : map (parens . pprConPatArg) binds)

    pprPrec _ (AsNPat v _ WildNPat) =
        ppr v

    pprPrec _ (AsNPat v ty pat) =
        parens (ppr v <+> text "::" <+/> ppr ty)
        <> text "@" <> pprPrec 11 pat
\end{code}
