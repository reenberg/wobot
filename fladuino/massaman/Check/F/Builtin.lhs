\section{Builtin Type Constructors and Types}

%if False
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
-- Module      :  Check.F.Builtin
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.F.Builtin where

import Data.Loc
import Data.Name
import Language.F
import Text.PrettyPrint.Mainland ()
\end{code}
%endif

\begin{code}
alpha :: WildTyVar
alpha = TameTyVar (TyVar (mkName "a"))

a :: Type
a = TyVarTy (TyVar (mkName "a")) builtinLoc
\end{code}

\begin{code}
nilCon, consCon :: Con
nilCon   = Con builtinNil
consCon  = Con builtinCons

unitTyCon, integerTyCon, floatTyCon, boolTyCon, charTyCon, listTyCon :: TyCon
unitTyCon     = TupleTyCon 0
integerTyCon  = TyCon prelInteger
floatTyCon    = TyCon prelFloat
boolTyCon     = TyCon prelBool
charTyCon     = TyCon prelChar
listTyCon     = TyCon builtinNil
arrowTyCon    = TyCon builtinArrow

unitTy, integerTy, floatTy, boolTy, charTy, stringTy :: Type
unitTy     = TyConTy unitTyCon builtinLoc
integerTy  = TyConTy integerTyCon builtinLoc
floatTy    = TyConTy floatTyCon builtinLoc
boolTy     = TyConTy boolTyCon builtinLoc
charTy     = TyConTy charTyCon builtinLoc
stringTy   = listTy charTy

listTy :: Type -> Type
listTy ty = AppTy (TyConTy listTyCon builtinLoc) ty builtinLoc
\end{code}

\begin{code}
builtinTyCons :: [(TyCon, Kind)]
builtinTyCons = [(integerTyCon,  (:*)),
                 (floatTyCon,    (:*)),
                 (listTyCon,     (:*) :=> (:*)),
                 (arrowTyCon,    (:*) :=> (:*) :=> (:*))
                ]

builtinConstructors :: [(Con, Type)]
builtinConstructors = [(nilCon,   ForAll alpha (:*) (listTy a) builtinLoc),
                       (consCon,  ForAll alpha (:*) (a --> listTy a --> listTy a) builtinLoc)
                      ]

builtinTyConCons :: [(TyCon, [Con])]
builtinTyConCons = [(listTyCon, [nilCon, consCon])]

builtinConTyCons :: [(Con, TyCon)]
builtinConTyCons = [(nilCon,   listTyCon),
                    (consCon,  listTyCon)
                   ]

builtinVars :: [(Var, Type)]
builtinVars = [(Var prelUndefined,  ForAll alpha (:*) a builtinLoc),
               (Var prelError,      ForAll alpha (:*) (stringTy --> a) builtinLoc)
              ]

mkUndefined :: Type -> Exp
mkUndefined ty = TyAppExp (VarExp (Var prelUndefined) sloc) ty sloc
  where
    sloc = SrcLoc internalLoc

mkError :: Type -> String -> Exp
mkError ty s = AppExp  (TyAppExp (VarExp (Var prelError) sloc) ty sloc)
                       (LitExp (StringLit s) sloc)
                       sloc
  where
    sloc = SrcLoc internalLoc
\end{code}
