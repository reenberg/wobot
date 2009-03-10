%if style == code
\begin{code}
{
{-# OPTIONS -w #-}
{-# OPTIONS -fglasgow-exts #-}

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
-- Module      :  Language.F.Parser.Parser
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.F.Parser.Parser (
    parseBody,
    parseType,
    parseExp
  ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

import Compiler.Opt
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.F.Parser.Exceptions
import Language.F.Parser.Monad
import Language.F.Parser.Lexer
import Language.F.Parser.Tokens
import Language.F
import Text.PrettyPrint.Mainland
}
\end{code}
%endif

\section{Parser}

\begin{code}
%token
  INTEGER  { L _ (Tinteger _) }
  FLOAT    { L _ (Tfloat _) }
  CHAR     { L _ (Tchar _) }
  STRING   { L _ (Tstring _) }

  VARID   { L _ (Tqvarid []  _) }
  QVARID  { L _ (Tqvarid _   _) }
  CONID   { L _ (Tqconid []  _) }
  QCONID  { L _ (Tqconid _   _) }

  VARSYM   { L _ (Tqvarsym []  _) }
  QVARSYM  { L _ (Tqvarsym _   _) }
  CONSYM   { L _ (Tqconsym []  _) }
  QCONSYM  { L _ (Tqconsym _   _) }

  'axiom'     { L _ Taxiom }
  'case'      { L _ Tcase }
  'data'      { L _ Tdata }
  'import'    { L _ Timport }
  'in'        { L _ Tin }
  'left'      { L _ Tleft }
  'let'       { L _ Tlet }
  'letrec'    { L _ Tletrec }
  'module'    { L _ Tmodule }
  'of'        { L _ Tof }
  'right'     { L _ Tright }
  'sym'       { L _ Tsym }
  'type'      { L _ Ttype }
  'where'     { L _ Twhere }
  '_'         { L _ Tunderscore }

  '.'    { L _ Tdot }
  ':'    { L _ Tcolon }
  '::'   { L _ Tdcolon }
  '='    { L _ Tequal }
  '\\'   { L _ Tlam }
  '/\\'  { L _ TLam }
  '->'   { L _ Trarrow }
  '=>'   { L _ Tdarrow }
  '!'    { L _ Tbang }
  '*'    { L _ Tstar }
  '\/'   { L _ Tforall }

  '~'    { L _ Tcosim }
  'o'    { L _ Tcotrans }
  '@'    { L _ Tcoapp }
  '|>'   { L _ Tcast }

  '('  { L _ Tlparen }
  ')'  { L _ Trparen }
  ','  { L _ Tcomma }
  ';'  { L _ Tsemi }
  '['  { L _ Tlbrack }
  ']'  { L _ Trbrack }
  '{'  { L _ Tlbrace }
  '}'  { L _ Trbrace }

%monad      { P } { >>= } { return }
%lexer      { lexer } { L _ Teof }
%tokentype  { (L Token) }
%error      { happyError }

%name  parseBody  body
%name  parseType  type
%name  parseExp   exp
%%
\end{code}

\begin{code}
literal :: { L Exp }
literal :
     INTEGER
       { locate (getLoc $1) $ LitExp $ IntegerLit (getINTEGER $1) }
  |  FLOAT
       { locate (getLoc $1) $ LitExp $ FloatLit (getFLOAT $1) }
  |  CHAR
       {locate (getLoc $1) $ LitExp $ CharLit (getCHAR $1) }
  |  STRING
       {locate (getLoc $1) $ LitExp $ StringLit (getSTRING $1) }

varid :: { L Name }
varid : VARID { locname (getLoc $1) (getVARID $1) }

qvarid :: { L Name }
qvarid :
     varid   { $1 }
  |  QVARID  { locqname (getLoc $1) (getQVARID $1) }

conid :: { L Name }
conid : CONID { locname (getLoc $1) (getCONID $1) }

qconid :: { L Name }
qconid :
     conid   { $1 }
  |  QCONID  { locqname (getLoc $1) (getQCONID $1) }

tyvar :: { L Name }
tyvar : varid { $1 }

tycon :: { L TyCon }
tycon : conid { reloc TyCon $1 }

qtycon :: { L TyCon }
qtycon : qconid { reloc TyCon $1 }

varsym :: { L Name }
varsym :
     VARSYM  { locname (getLoc $1) (getVARSYM $1) }
  |  '!'     { locname (getLoc $1) "!" }
  |  '*'     { locname (getLoc $1) "*" }
  |  '.'     { locname (getLoc $1) "." }

qvarsym :: { L Name }
qvarsym :
     varsym   { $1 }
  |  QVARSYM  { locqname (getLoc $1) (getQVARSYM $1) }

consym :: { L Name }
consym : CONSYM { locname (getLoc $1) (getCONSYM $1) }

qconsym :: { L Name }
qconsym :
     consym   { $1 }
  |  QCONSYM  { locqname (getLoc $1) (getQCONSYM $1) }
\end{code}

\subsection{Declarations}

\begin{code}
body :: { [L Decl] }
body :
    topdecls { reverse $1 }

-- List built in reverse order
topdecls :: { [L Decl] }
topdecls :
     topdecl
       { [$1] }
  |  topdecls topdecl
       { $2 : $1 }

topdecl :: { L Decl }
topdecl :
     'data' tycon '::' kind
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $
           DataDecl (unLoc $2) (unLoc $4) []
       }
  |  'data' tycon '::' kind 'where' '{' constrs '}'
       { let loc = combineLocs [getLoc $1, getLoc $8]
         in
           locate loc $
           DataDecl (unLoc $2) (unLoc $4) (reverse $ map unLoc $7)
       }
  |  'let' '{' binding '}'
       { let loc = combineLocs [getLoc $1, getLoc $2]
         in
           locate loc $
           LetDecl (NonRec $ unLoc $3)
       }
  |  'letrec' '{' bindings '}'
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $
           LetDecl (Rec $ reverse $ map unLoc $3)
       }
  |  var '::' type ';'
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $
           SigDecl (unLoc $1) (unLoc $3)
       }

-- List built in reverse order
bindings :: { [L Binding] }
bindings :
     {- empty -}
       { [] }
  |  binding
       { [$1] }
  |  bindings ';' binding
       { $3 : $1 }

binding :: { L Binding }
binding :
     var '::' type '=' exp
       { let loc = combineLocs [getLoc $1, getLoc $5]
         in
           locate loc $
           Binding (unLoc $1) (unLoc $3) defaultBindInfo (unLoc $5)
       }
\end{code}

\subsection{Kinds}

\begin{code}
kind :: { L Kind }
kind :
     kind0
       { $1 }
  |  kind0 '->' kind
       { let loc = getLoc $1 <--> getLoc $3
         in
           L loc $ unLoc $1 :=> unLoc $3
       }

kind0 :: { L Kind }
kind0 :
    '*'
      { L (getLoc $1) (:*) }
  |  '(' type '~' type ')'
       { let loc = getLoc $1 <--> getLoc $5
         in
           L loc $
           (unLoc $2) :~ (unLoc $4)
       }
  | '(' kind ')'
      { $2 }
\end{code}

\subsection{Types}

\begin{code}
type :: { L Type }
type :
    btype
      { $1 }
  | btype '->' type
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $
          AppTy  (AppTy  (TyConTy (TyCon builtinArrow) (SrcLoc loc))
                         (unLoc $1)
                         (SrcLoc loc))
                 (unLoc $3)
      }
  | '\/' tyvar_bind_list '.' type
      { let  {  loc = combineLocs [getLoc $1, getLoc $4]
             ;  f (tv, k) ty = ForAll tv k ty (SrcLoc (getLoc ty))
             }
        in
          L loc $
          foldr f (unLoc $4) (reverse $ map unLoc $2)
      }
  | btype 'o' btype
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          locate loc $
          TransCo (unLoc $1) (unLoc $3)
      }
  | btype '@' btype
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          locate loc $
          AppCo (unLoc $1) (unLoc $3)
      }
  | 'sym' btype
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          locate loc $
          SymCo (unLoc $2)
      }
  | 'left' btype
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          locate loc $
          LeftCo (unLoc $2)
      }
  | 'right' btype
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          locate loc $
          RightCo (unLoc $2)
      }
  | kind '=>' type
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          locate loc $
          ForAll WildTyVar (unLoc $1) (unLoc $3)
      }

types :: { [L Type] }
types :
     {- empty -}  { [] }
  |  type         { [$1] }
  |  types2       { reverse $1 }

-- List built in reverse order
types1 :: { [L Type] }
types1 :
     type             { [$1] }
  |  types1 ',' type  { $3 : $1 }

-- List built in reverse order
types2 :: { [L Type] }
types2 :
     types1 ',' type  { $3 : $1 }

tyvar_bind :: { L (WildTyVar, Kind) }
tyvar_bind :
     tyvar
       { L (getLoc $1) $ (TameTyVar $ TyVar (unLoc $1), (:*)) }
  |  tyvar '::' kind
       { let loc = combineLocs [getLoc $1, getLoc $3]
         in
           L loc $ (TameTyVar $ TyVar $ (unLoc $1), unLoc $3)
       }
  |  '(' tyvar_bind ')'
       { $2 }

-- List built in reverse order
tyvar_bind_list :: { [L (WildTyVar, Kind)] }
tyvar_bind_list :
     {- empty -}                             { [] }
  |  tyvar_bind                          { [$1] }
  |  tyvar_bind_list ',' tyvar_bind  { $3 : $1 }

btype :: { L Type }
btype :
    atype
      { $1 }
  | btype atype
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          locate loc $ AppTy (unLoc $1) (unLoc $2)
      }

atype :: { L Type }
atype :
    gtycon
      { $1 }
  | tyvar
      { let loc = getLoc $1
        in
          locate loc $ TyVarTy (TyVar (unLoc $1))
      }
  | '(' types2 ')'
      {  let  {  loc  = getLoc $1 <--> getLoc $3
              ;  tys  = reverse (map unLoc $2)
              }
         in
           L loc $
           appsT (TyConTy (TupleTyCon (length tys)) (SrcLoc loc)) tys
      }
  | '[' type ']'
      {  let loc = getLoc $1 <--> getLoc $3
         in
           L loc $
           AppTy (TyConTy (TyCon builtinNil) (SrcLoc loc)) (unLoc $2) (SrcLoc loc)
      }
  | '(' type ')'
      { $2 }

gtycon :: { L Type }
gtycon :
    qtycon
      { let loc = getLoc $1
        in
          locate loc $
          TyConTy (unLoc $1)
      }
  | '(' ')'
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          locate loc $
          TyConTy (TupleTyCon 0)
      }
  | '[' ']'
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          locate loc $
          TyConTy (TyCon builtinNil)
      }
  | '(' '->' ')'
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          locate loc $
          TyConTy (TyCon (name (getLoc $2) "->"))
      }
  | '(' ',' commas ')'
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          locate loc $
          TyConTy (TupleTyCon (2 + $3))
      }

commas :: { Int }
commas :
    {- empty -}  { 0 }
  | ',' commas   { 1 + $2 }

-- List built in reverse order
constrs :: { [L ConDecl] }
constrs :
    constr              { [$1] }
  | constrs ';' constr  { $3 : $1 }
\end{code}

\begin{code}
constr :: { L ConDecl }
constr :
     con '::' type
       { let loc = combineLocs [getLoc $1, getLoc $3]
         in
           locate loc $ ConDecl (unLoc $1) (unLoc $3) []
       }
  |  con '{' field_list '}' '::' type
       { let loc = combineLocs [getLoc $1, getLoc $6]
         in
           locate loc $
           ConDecl  (unLoc $1) (unLoc $6)
                    (reverse $ map unLoc $3)
       }

field_list :: { [L Var] }
field_list :
     {- empty -}         { [] }
  |  var                 { [$1] }
  |  field_list ',' var  { $3 : $1 }
\end{code}

\subsection{Expressions}

\begin{code}
exp :: { L Exp }
exp :
    exp0b '|>' type
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          locate loc $
          CastExp (unLoc $1) (unLoc $3)
      }
  | exp0
      { $1 }

exp0 :: { L Exp }
exp0 :
     exp0a  { $1 }
  |  exp0b  { $1 }

exp0a :: { L Exp }
exp0a :
    '\\' var '::' type '.' exp
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          locate loc $
          LamExp (unLoc $2) (unLoc $4) (unLoc $6)
      }
  | '/\\' tyvar '.' exp
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          locate loc $
          TyLamExp (TyVar $ (unLoc $2)) (:*) (unLoc $4)
      }
  | '/\\' tyvar '::' kind '.' exp
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          locate loc $
          TyLamExp (TyVar $ (unLoc $2)) (unLoc $4) (unLoc $6)
      }
  | 'let' '{' binding '}' 'in' exp
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          locate loc $
          LetExp (NonRec $ unLoc $3) (unLoc $6)
      }
  | 'letrec' '{' bindings '}' 'in' exp
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          locate loc $
          LetExp (Rec $ reverse $ map unLoc $3) (unLoc $6)
      }

exp0b :: { L Exp }
exp0b :
    'case' exp 'of' var '{' alts '}'
      { let loc = combineLocs [getLoc $1, getLoc $7]
        in
          locate loc $
          CaseExp (unLoc $2) (unLoc $4) (reverse $ map unLoc $6)
      }
  | fexp
      { $1 }

fexp :: { L Exp }
fexp :
     aexp
       { $1 }
  |  fexp '[' type ']'
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $ TyAppExp (unLoc $1) (unLoc $3)
       }
  |  fexp aexp
       { let loc = combineLocs [getLoc $1, getLoc $2]
         in
           locate loc $ AppExp (unLoc $1) (unLoc $2)
       }

aexp :: {L Exp }
aexp :
    qvar
      { locate (getLoc $1) $ VarExp (unLoc $1) }
  | gcon
      { locate (getLoc $1) $ ConExp (unLoc $1) }
  | literal
      { $1 }
  | '(' exp ')'
      { $2 }
  | '(' exp ',' exp_list ')'
      { let  {  loc     = combineLocs [getLoc $1, getLoc $5]
             ;  exps    = unLoc $2 : reverse (map unLoc $4)
             ;  conExp  = ConExp (TupleCon (length exps)) (fromLoc loc)
             }
        in
          L loc $
          appsE conExp exps
      }

-- List built in reverse order
exp_list :: { [L Exp] }
exp_list :
    exp               { [$1] }
  | exp_list ',' exp  { $3 : $1 }

alt :: { L Alt }
alt :
     pat '->' exp
       { let loc = combineLocs [getLoc $1, getLoc $3]
         in
           L loc $ Alt (unLoc $1) (unLoc $3)
       }

-- List built in reverse order
alts :: { [L Alt] }
alts :
    alt           { [$1] }
  | alts ';' alt  { $3 : $1 }
  | alts ';' ';'  { $1 }
\end{code}

\subsection{Patterns}

\begin{code}
patvar :: { L (WildVar, Type) }
patvar :
     '(' varid '::' type ')'
       { let loc = combineLocs [getLoc $1, getLoc $5]
         in
           L loc $ (TameVar $ Var (unLoc $2), unLoc $4)
       }
  |  '(' '_' '::' type ')'
       { let loc = combineLocs [getLoc $1, getLoc $5]
         in
           L loc $ (WildVar, unLoc $4)
       }

-- List built in reverse order
patvars :: { [L (WildVar, Type)] }
patvars :
     {- empty -}     { [] }
  |  patvar          { [$1] }
  |  patvars patvar  { $2 : $1 }

pattyvar :: { L (WildTyVar, Kind) }
pattyvar :
     '[' varid '::' kind ']'
       { let loc = combineLocs [getLoc $1, getLoc $5]
         in
           L loc $ (TameTyVar $ TyVar (unLoc $2), unLoc $4)
       }
  |  '[' '_' '::' kind ']'
       { let loc = combineLocs [getLoc $1, getLoc $5]
         in
           L loc $ (WildTyVar, unLoc $4)
       }

-- List built in reverse order
pattyvars :: { [L (WildTyVar, Kind)] }
pattyvars :
     {- empty -}         { [] }
  |  pattyvar            { [$1] }
  |  pattyvars pattyvar  { $2 : $1 }

pat :: { L Pat }
pat :
     INTEGER
       { locate (getLoc $1) $ LitPat $ IntegerLit (getINTEGER $1) }
  |  FLOAT
       { locate (getLoc $1) $ LitPat $ FloatLit (getFLOAT $1) }
  |  CHAR
       { locate (getLoc $1) $ LitPat $ CharLit (getCHAR $1) }
  |  STRING
       { locate (getLoc $1) $ LitPat $ StringLit (getSTRING $1) }
  |  patvar
       { locate (getLoc $1) $ VarPat (unLoc $1) }
{- This production generates two shift-reduce conflicts -}
  |  gcon pattyvars patvars
       { let loc = combineLocs (getLoc $1 : map getLoc $3)
         in
           locate loc $ ConPat  (unLoc $1)
                                (reverse $ map unLoc $2)
                                (reverse $ map unLoc $3)
       }
\end{code}

\begin{code}
gcon :: { L Con }
gcon :
    '(' ')'
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          L loc $ TupleCon 0
      }
  | '[' ']'
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          L loc $ Con builtinNil
      }
  | '(' ',' commas ')'
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          L loc $ TupleCon (2 + $3)
      }
  | qcon
      { $1 }

var :: { L Var }
var :
    varid           { L (getLoc $1) $ Var (unLoc $1) }
  | '(' varsym ')'  { L (getLoc $2) $ Var (unLoc $2) }

qvar :: { L Var }
qvar :
    qvarid           { L (getLoc $1) $ Var (unLoc $1) }
  | '(' varsym ')'   { L (getLoc $2) $ Var $ unLoc $2 }
  | '(' QVARSYM ')'  { L (getLoc $2) $ Var $ qname (getLoc $2) (getQVARSYM $2) }

con :: { L Con }
con :
    conid           { L (getLoc $1) $ Con (unLoc $1) }
  | '(' consym ')'  { L (getLoc $2) $ Con (unLoc $2) }

qcon :: { L Con }
qcon :
    qconid           { L (getLoc $1) $ Con (unLoc $1) }
  | '(' gconsym ')'  { $2 }

gconsym :: { L Con }
gconsym :
    ':'      { L (getLoc $1) $ Con (name (getLoc $1) ":") }
  | qconsym  { L (getLoc $1) $ Con (unLoc $1) }
\end{code}

\begin{code}
{
\end{code}

\subsection{Lexing}

\begin{code}
lexer :: (MonadParser m) => (L Token -> m a) -> m a
lexer cont = do
    tok <- token
    cont tok
  where
    token :: (MonadParser m) => m (L Token)
    token = do
        ts <- getTokens
        case ts of
          []          ->  return (L internalLoc Teof)
          (t : rest)  ->  setTokens rest >> return t
\end{code}

\begin{code}
locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))
\end{code}

\begin{code}
reloc :: (a -> b) -> L a -> L b
reloc f a = L (getLoc a) $ f (unLoc a)
\end{code}

\begin{code}
name :: Loc -> String -> Name
name loc s = mkUnqualName s loc

qname :: Loc -> ([String], String) -> Name
qname loc (q, s) = mkQualName q s loc

locname :: Loc -> String -> L Name
locname loc s = L loc $ mkUnqualName s loc

locqname :: Loc -> ([String], String) -> L Name
locqname loc (q, s) = L loc $ mkQualName q s loc
\end{code}

\begin{code}
happyError :: MonadParser m => L Token -> m a
happyError (L (Loc start _) _) = throwExceptionAt start ParserError

getCHAR     (L _ (Tchar x))     = x
getCHAR     _                   = internalErr $ text "not a CHAR"

getSTRING   (L _ (Tstring x))   = x
getSTRING   _                   = internalErr $ text "not a STRING"

getINTEGER  (L _ (Tinteger x))  = x
getINTEGER  _                   = internalErr $ text "not an INTEGER"

getFLOAT    (L _ (Tfloat x))    = x
getFLOAT    _                   = internalErr $ text "not a FLOAT"

getVARID   (L _ (Tqvarid [] x))  = x
getVARID    _                    = internalErr $ text "not a VARID"

getQVARID  (L _ (Tqvarid qs x))  = (qs, x)
getQVARID   _                    = internalErr $ text "not a VARID"

getCONID   (L _ (Tqconid [] x))  = x
getCONID   _                     = internalErr $ text "not a CONID"

getQCONID  (L _ (Tqconid qs x))  = (qs, x)
getQCONID   _                    = internalErr $ text "not a QCONID"

getVARSYM   (L _ (Tqvarsym [] x))  = x
getVARSYM   _                      = internalErr $ text "not a VARSYM"

getQVARSYM  (L _ (Tqvarsym qs x))  = (qs, x)
getQVARSYM  _                      = internalErr $ text "not a QVARSYM"

getCONSYM   (L _ (Tqconsym [] x))  = x
getCONSYM  _                       = internalErr $ text "not a CONSYM"

getQCONSYM  (L _ (Tqconsym qs x))  = (qs, x)
getQCONSYM  _                      = internalErr $ text "not a QCONSYM"
\end{code}

%if style == code
\begin{code}
}
\end{code}
%endif
