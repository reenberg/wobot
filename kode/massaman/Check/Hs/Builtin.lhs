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
-- Module      :  Check.Hs.Builtin
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.Hs.Builtin where

import Check.Hs.Types
import Data.Name
import qualified Language.Hs as H
\end{code}
%endif

\begin{code}
alpha :: (TyVar, Kind r)
alpha = (TyVar (mkName "a"), (:*))

a :: Type r
a = TyVarTy $ TyVar (mkName "a")
\end{code}

\begin{code}
nilCon, consCon :: H.Con
nilCon   = H.Con builtinNil
consCon  = H.Con builtinCons

unitTyCon, integerTyCon, floatTyCon, boolTyCon, charTyCon, listTyCon :: TyCon
unitTyCon     = TupleTyCon 0
integerTyCon  = TyCon prelInteger
floatTyCon    = TyCon prelFloat
boolTyCon     = TyCon prelBool
charTyCon     = TyCon prelChar
listTyCon     = TyCon builtinNil
arrowTyCon    = TyCon builtinArrow

hsListTyCon :: H.TyCon
hsListTyCon = H.TyCon builtinNil

tyconSigma :: TyCon -> Sigma r
tyconSigma tycon = ForAll [] [] (TyConTy tycon)

unitTy, integerTy, floatTy, boolTy, charTy, stringTy :: Sigma r
unitTy     = tyconSigma unitTyCon
integerTy  = tyconSigma integerTyCon
floatTy    = tyconSigma floatTyCon
boolTy     = tyconSigma boolTyCon
charTy     = tyconSigma charTyCon
stringTy   = listTy (TyConTy charTyCon)

listTy :: Type r -> Type r
listTy ty = AppTy (TyConTy listTyCon) ty
\end{code}

\begin{code}
builtinTyCons :: [(H.TyCon, Kind r)]
builtinTyCons = [(H.TyCon builtinNil,    (:*) :=> (:*)),
                 (H.TyCon builtinArrow,  (:*) :=> (:*) :=> (:*))
                ]

builtinConstructors :: [(H.Con, Sigma r)]
builtinConstructors = [(nilCon,   ForAll [alpha] [] (listTy a)),
                       (consCon,  ForAll [alpha] [] (a --> listTy a --> listTy a))
                      ]

builtinTyConCons :: [(H.TyCon, [H.Con])]
builtinTyConCons = [(hsListTyCon, [nilCon, consCon])]

builtinConTyCons :: [(H.Con, H.TyCon)]
builtinConTyCons = [(nilCon,   hsListTyCon),
                    (consCon,  hsListTyCon)
                   ]

builtinVars :: [(H.Var, Sigma r)]
builtinVars = [(H.Var prelUndefined,  ForAll [alpha] [] a),
               (H.Var prelError,      ForAll [alpha] [] (stringTy --> a))
              ]
\end{code}
