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
-- Module      :  Language.Hs.Parser.Parser
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Parser.Parser (
    parseModule,
    parseBody,
    parseTopDecls,
    parseDecls,
    parseType,
    parseExp,
    parseStmt
  ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (foldl1')
import qualified Data.Map as Map

import Compiler.Opt
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.Hs
import Language.Hs.Parser.Exceptions
import Language.Hs.Parser.Layout
import Language.Hs.Parser.Lexer
import Language.Hs.Parser.Monad
import Language.Hs.Parser.Tokens
import Language.Hs.Parser.Utils
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

  'case'      { L _ Tcase }
  'class'     { L _ Tclass }
  'data'      { L _ Tdata }
  'default'   { L _ Tdefault }
  'deriving'  { L _ Tderiving }
  'do'        { L _ Tdo }
  'else'      { L _ Telse }
  'if'        { L _ Tif }
  'import'    { L _ Timport }
  'in'        { L _ Tin }
  'infix'     { L _ Tinfix }
  'infixl'    { L _ Tinfixl }
  'infixr'    { L _ Tinfixr }
  'instance'  { L _ Tinstance }
  'let'       { L _ Tlet }
  'module'    { L _ Tmodule }
  'newtype'   { L _ Tnewtype }
  'of'        { L _ Tof }
  'then'      { L _ Tthen }
  'type'      { L _ Ttype }
  'where'     { L _ Twhere }
  '_'         { L _ Tunderscore }

  'as'         { L _ Tas }
  'qualified'  { L _ Tqualified }
  'hiding'     { L _ Thiding }

  '..'  { L _ Tdotdot }
  ':'   { L _ Tcolon }
  '::'  { L _ Tdcolon }
  '='   { L _ Tequal }
  '\\'  { L _ Tlam }
  '|'   { L _ Tvbar }
  '<-'  { L _ Tlarrow }
  '->'  { L _ Trarrow }
  '@'   { L _ Tat }
  '~'   { L _ Ttilda }
  '=>'  { L _ Tdarrow }
  '-'   { L _ Tminus }
  '!'   { L _ Tbang }

  '('  { L _ Tlparen }
  ')'  { L _ Trparen }
  ','  { L _ Tcomma }
  ';'  { L _ Tsemi }
  '['  { L _ Tlbrack }
  ']'  { L _ Trbrack }
  '`'  { L _ Tbackquote }
  '{'  { L _ Tlbrace }
  '}'  { L _ Trbrace }

  ANTI_ID     { L _ (Tanti_id _) }
  ANTI_INT    { L _ (Tanti_int _) }
  ANTI_FLOAT  { L _ (Tanti_float _) }
  ANTI_VAR    { L _ (Tanti_var _) }
  ANTI_TYPE   { L _ (Tanti_type _) }
  ANTI_EXP    { L _ (Tanti_exp _) }

%monad      { P } { >>= } { return }
%lexer      { lexer } { L _ Teof }
%tokentype  { (L Token) }
%error      { happyError }

%name  parseModule    module
%name  parseBody      body
%name  parseTopDecls  topdecls
%name  parseDecls     decls
%name  parseType      type
%name  parseExp       exp
%name  parseStmt      stmt
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
       { locate (getLoc $1) $ LitExp $ CharLit (getCHAR $1) }
  |  STRING
       { locate (getLoc $1) $ LitExp $ StringLit (getSTRING $1) }
  |  ANTI_INT
       { locate (getLoc $1) $ LitExp $ AntiInt (getANTI_INT $1) }
  |  ANTI_FLOAT
       { locate (getLoc $1) $ LitExp $ AntiFloat (getANTI_FLOAT $1) }

varid :: { L Name }
varid :
      VARID       { locname (getLoc $1) (getVARID $1) }
  |  'as'         { locname (getLoc $1) "as" }
  |  'hiding'     { locname (getLoc $1) "hiding" }
  |  'qualified'  { locname (getLoc $1) "qualified" }

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

tycon :: { L Name }
tycon : conid { $1 }

tycls :: { L Name }
tycls : conid { $1 }

modid :: { L Name }
modid : QCONID { locname (getLoc $1) (getCONID $1) }

qtycon :: { L Name }
qtycon : qconid { $1 }

qtycls :: { L Name }
qtycls : qconid { $1 }

varsym :: { L Name }
varsym :
     varsym_no_minus  { $1 }
  |  '-'              { locname (getLoc $1) "-" }
  |  '!'              { locname (getLoc $1) "!" }

varsym_no_minus :: { L Name }
varsym_no_minus : VARSYM { locname (getLoc $1) (getVARSYM $1) }

qvarsym :: { L Name }
qvarsym :
     varsym   { $1 }
  |  QVARSYM  { locqname (getLoc $1) (getQVARSYM $1) }

qvarsym_no_minus :: { L Name }
qvarsym_no_minus :
     varsym_no_minus  { $1 }
  |  QVARSYM          { locqname (getLoc $1) (getQVARSYM $1) }

consym :: { L Name }
consym : CONSYM { locname (getLoc $1) (getCONSYM $1) }

qconsym :: { L Name }
qconsym :
     consym   { $1 }
  |  QCONSYM  { locqname (getLoc $1) (getQCONSYM $1) }
\end{code}

\subsection{Modules and Imports}

\begin{code}
module :: { Module }
module :
     'module' modid maybe_exports 'where' body
       { Module (unLoc $2) $3 (unLoc $5) }
  |  body
       { Module (name (getLoc $1) "Main") [] (unLoc $1) }

maybe_exports :: { [Export] }
maybe_exports :
     {- empty -}  { [] }
  |  exports      { $1 }

exports :: { [Export] }
exports :
     '(' export_list ')' { reverse $ map unLoc $2 }

-- List built in reverse order
export_list :: { [L Export] }
export_list :
     {- empty -}             { [] }
  |  export                  { [$1] }
  |  export_list ',' export  { $3 : $1 }
  |  export_list ','         { $1 }

export :: { L Export }
export :
     var
       { L (getLoc $1) $ VarExport (unLoc $1) }
  |  tycon '(' '..' ')'
       {  let loc = getLoc $1 <--> getLoc $4
          in
            L loc $ TyConExport (TyCon $ unLoc $1) Nothing
       }
  |  tycon '(' cname_list ')'
       {  let loc = getLoc $1 <--> getLoc $4
          in
            L loc $
            TyConExport (TyCon $ unLoc $1) (Just (reverse $ map unLoc $3))
       }
  |  'module' modid
       { L (getLoc $1) $ ModuleExport (unLoc $2) }

impdecl  :: { L ImportDecl }
impdecl :
     'import' maybe_qualified modid maybe_as impspec
       {  let loc = combineLocs (getLoc $1 : map getLoc (snd $5))
          in
            locate loc $
            ImportDecl $2 (unLoc $3) $4 (fst $5) (map unLoc (snd $5))
       }

-- List built in reverse order
impdecls  :: { [L ImportDecl] }
impdecls :
     ','                   { [] }
  |  impdecl               { [$1] }
  |  impdecls ',' impdecl  { $3 : $1 }

maybe_qualified :: { Bool }
maybe_qualified :
     {- empty -}  { False}
  |  'qualified'  { True }

maybe_as :: { Maybe Name }
maybe_as :
     {- empty -} { Nothing }
  |  'as' modid  { Just (unLoc $2) }

impspec :: { (Bool, [L Import]) }
impspec :
     '(' import_list ')'           { (False, reverse $2) }
  |  'hiding' '(' import_list ')'  { (True, reverse $3) }

-- List built in reverse order
import_list :: { [L Import] }
import_list :
     {- empty -}             { [] }
  |  import                  { [$1] }
  |  import_list ',' import  { $3 : $1 }
  |  import_list ','         { $1 }

import :: { L Import }
import :
     qvar
       { locate (getLoc $1) $ VarImport (unLoc $1) }
  |  qtycon '(' '..' ')'
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $ TyConImport (TyCon $ unLoc $1) Nothing
       }
  |  qtycon '(' cname_list ')'
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $
           TyConImport (TyCon $ unLoc $1) (Just (reverse $ map unLoc $3))
       }

-- List built in reverse order
cname_list :: { [L CName] }
cname_list :
     {- empty -}           { [] }
  |  cname                 { [$1] }
  |  cname_list ',' cname  { $3 : $1 }

cname :: { L CName }
cname :
     var  { L (getLoc $1) $ VarCName (unLoc $1) }
  |  con  { L (getLoc $1) $ ConCName (unLoc $1) }

\end{code}

\subsection{Declarations}

\begin{code}
body :: { L Body }
body :
     '{' impdecls ';' topdecls '}'
       {  let loc = combineLocs [getLoc $1, getLoc $5]
          in
            L loc $
            Body (reverse $ map unLoc $2) (reverse $ map unLoc $4)
       }
  |  '{' impdecls ';' topdecls error
       {%  do  {  lookForRightBrace
               ;  let loc = combineLocs (getLoc $1 : map getLoc $4)
               ;  return $ L loc $
                  Body (reverse $ map unLoc $2) (reverse $ map unLoc $4)
               }
       }
  |  '{' impdecls '}'
       {  let loc = combineLocs [getLoc $1, getLoc $3]
          in
            L loc $
            Body (reverse $ map unLoc $2) []
       }
  |  '{' impdecls error
       {%  do  {  lookForRightBrace
               ;  let loc = combineLocs (getLoc $1 : map getLoc $2)
               ;  return $ L loc $
                  Body (reverse $ map unLoc $2) []
               }
       }
  |  '{' topdecls '}'
       {  let loc = combineLocs [getLoc $1, getLoc $3]
          in
            L loc $
            Body [] (reverse $ map unLoc $2)
       }
  |  '{' topdecls error
       {%  do  {  lookForRightBrace
               ;  let loc = combineLocs (getLoc $1 : map getLoc $2)
               ;  return $ L loc $
                  Body [] (reverse $ map unLoc $2)
               }
       }

-- List built in reverse order
topdecls :: { [L Decl] }
topdecls :
     topdecl
       { [$1] }
  |  topdecls ';'
       { $1 }
  |  topdecls ';' topdecl
       { $3 : $1 }

topdecl :: { L Decl }
topdecl :
     'type' type '=' type
       {% do  {  let loc          =   combineLocs [getLoc $1, getLoc $4]
              ;  (tycon, tyvars)  <-  checkData (unLoc $2)
              ;  return $ locate loc $
                 TypeDecl tycon tyvars (unLoc $4)
              }
       }
  |  'data' ctype deriving
       {% do  {  let loc          =   combineLocs (getLoc $1 : map getLoc $3)
              ;  let (ctx, ty)    =   unLoc $2
              ;  (tycon, tyvars)  <-  checkData ty
              ;  let derives      =   map unLoc $3
              ;  return $ locate loc $
                 DataDecl DataType ctx tycon tyvars [] derives
              }
       }
  |  'data' ctype '=' constrs deriving
       {% do  {  let loc          =   combineLocs (getLoc $1 : map getLoc $5)
              ;  let (ctx, ty)    =   unLoc $2
              ;  (tycon, tyvars)  <-  checkData ty
              ;  let constrs      =   reverse $ map unLoc $4
              ;  let derives      =   map unLoc $5
              ;  return $ locate loc $
                 DataDecl DataType ctx tycon tyvars constrs derives
              }
       }
  |  'newtype' ctype '=' newconstr deriving
       {% do  {  let loc          =   combineLocs (getLoc $1 : map getLoc $5)
              ;  let (ctx, ty)    =   unLoc $2
              ;  (tycon, tyvars)  <-  checkData ty
              ;  let derives      =   map unLoc $5
              ;  return $ locate loc $
                 DataDecl NewType ctx tycon tyvars [unLoc $4] derives
              }
       }
  |  'class' ctype wherebinds
       {% do  {  let loc          =   combineLocs (getLoc $1 : map getLoc $3)
              ;  let (ctx, ty)    =   unLoc $2
              ;  (tycls, tyvars)  <-  checkClass ty
              ;  let decls        =   map unLoc $3
              ;  return $ locate loc $
                 ClassDecl ctx tycls tyvars decls
              }
       }
  |  'instance' ctype wherebinds
       {% do  {  let loc        =   combineLocs (getLoc $1 : map getLoc $3)
              ;  let (ctx, ty)  =   unLoc $2
              ;  (tycls, tys)   <-  checkInstance ty
              ;  let decls      =   map unLoc $3
              ;  return $ locate loc $
                 InstDecl ctx tycls tys decls
              }
       }
  |  'default' '(' types ')'
       { let  {  loc = combineLocs [getLoc $1, getLoc $4]
              }
         in
           locate loc $ DefaultDecl (map unLoc $3)
       }
  |  decl
       { $1 }
\end{code}

\begin{code}
context :: { L [Pred] }
context :
    btype
      {% do  {  let loc = getLoc $1
             ;  ctx <- checkContext (unLoc $1)
             ;  return $ L loc ctx
             }
      }

ctype :: { L ([Pred], Type) }
ctype :
     type
       {
         L (getLoc $1) ([], unLoc $1)
       }
  |  context '=>' type
       {% do  {  let loc = combineLocs [getLoc $1, getLoc $3]
              ;  return $ L loc (unLoc $1, unLoc $3)
              }
       }
\end{code}

\begin{code}
decls :: { [L Decl] }
decls :
     '{' decls_ '}'
       { reverse $2 }
  |  '{' decls_ error
       {% do  {  lookForRightBrace
              ;  return $ reverse $2
              }
       }

-- List built in reverse order
decls_ :: { [L Decl] }
decls_ :
     {- empty -}
       { [] }
  |  decl
       { [$1] }
  |  decls_ ';' decl
       { $3 : $1 }
\end{code}

We can't fully parse declarations when we first see them because they can
contain infix operators, and we can't know the correct fixity of operators until
we have parsed the whole file since fixity declarations can come \emph{after}
the definitions to which they are attached.

\begin{code}
decl :: { L Decl }
decl :
     gendecl
       { $1 }
  |  exp0b rhs
       {  locate (combineLocs [getLoc $1, getLoc $2]) $
          ExpBindDecl (unLoc $1) (unLoc $2)
       }
\end{code}

There's an awkward overlap with a type signature.  Consider

{\tt f :: Int -> Int = ...rhs...}

Then we can't tell whether it's a type signature or a value definition with a
result signature until we see the '='.  So we have to inline enough to postpone
reductions until we know.

\begin{code}
gendecl :: { L Decl }
gendecl :
    vars '::' ctype
      {% do  {  let loc         =  combineLocs (getLoc $3 : map getLoc $1)
             ;  let vars        =  reverse $ map unLoc $1
             ;  let (preds, ty)  =  unLoc $3
             ;  return $ locate loc $
                    SigDecl vars (ForAll ImplicitForAll [] preds ty)
             }
      }
  | fixity ops
      {% do  {  let loc = combineLocs $ getLoc $1 : map getLoc $2
             ;  let fixity = unLoc $1
             ;  let prec = fromInteger 9
             ;  let ops = map unLoc $2
             ;  return $ locate loc $ FixityDecl ops fixity prec
             }
      }
  | fixity INTEGER ops
      {% do  {  let loc     = combineLocs (getLoc $1 : map getLoc $3)
             ;  let fixity  = unLoc $1
             ;  let prec    = fromInteger $ getINTEGER $2
             ;  let ops     = map unLoc $3
             ;  return $ locate loc $ FixityDecl ops fixity prec
             }
      }

-- List built in reverse order
ops :: { [L Name] }
ops :
    op          { [$1] }
  | ops ',' op  { $3 : $1 }

-- List built in reverse order
vars :: { [L Var] }
vars :
    qvar          { [$1] }
  | vars ',' var  { $3 : $1 }

fixity :: { L Fixity }
fixity :
    'infix'   { L (getLoc $1) Infix }
  | 'infixl'  { L (getLoc $1) Infixl }
  | 'infixr'  { L (getLoc $1) Infixr }
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
          L loc $
          AppTy  (AppTy  (TyConTy (TyCon builtinArrow))
                         (unLoc $1))
                 (unLoc $3)
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

btype :: { L Type }
btype :
    atype
      { $1 }
  | btype atype
      { let loc = getLoc $1 <--> getLoc $2
        in
          L loc $ AppTy (unLoc $1) (unLoc $2)
      }
  | ANTI_TYPE
      { L (getLoc $1) $ AntiType (getANTI_TYPE $1) }

atype :: { L Type }
atype :
    gtycon
      { L (getLoc $1) $ TyConTy (unLoc $1) }
  | tyvar
      { L (getLoc $1) $ TyVarTy (TyVar (unLoc $1)) }
  | '(' types2 ')'
      {  let  {  loc  = combineLocs [getLoc $1, getLoc $3]
              ;  tys  = reverse (map unLoc $2)
              }
         in
           L loc $
           foldl AppTy (TyConTy (TupleTyCon (length tys))) tys
      }
  | '[' type ']'
      {  let loc = getLoc $1 <--> getLoc $3
         in
           L loc $
           AppTy (TyConTy (TyCon builtinNil)) (unLoc $2)
      }
  | '(' type ')'
      { $2 }

gtycon :: { L TyCon }
gtycon :
    qtycon
      {  L (getLoc $1) $
         TyCon (unLoc $1)
      }
  | '(' ')'
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          L loc $
          TupleTyCon 0
      }
  | '[' ']'
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          L loc $
          TyCon builtinNil
      }
  | '(' '->' ')'
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          L loc $
          TyCon (name loc "->")
      }
  | '(' ',' commas ')'
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          L loc $
          TupleTyCon (2 + $3)
      }

commas :: { Int }
commas :
    {- empty -}  { 0 }
  | ',' commas   { 1 + $2 }
\end{code}

\begin{code}
-- List built in reverse order
constrs :: { [L ConDecl] }
constrs :
    constr              { [$1] }
  | constrs '|' constr  { $3 : $1 }
\end{code}

We parse constructor declaration {\tt C t1 t2} as a $btype$ and then convert to
a data constructor to avoid reduce/reduce conflicts.

\begin{code}
constr :: { L ConDecl }
constr :
     btype
       {% do  {  let loc = getLoc $1
              ;  let (ty : tys) = unfoldAppTy (unLoc $1)
              ;  case ty : tys of
                   {  TyConTy (TyCon tycon) : tys ->
                          return $ locate loc $
                          ConDecl (Con tycon) defaultFixity tys
                   ;  _ ->
                       throwException BadPat
                   }
              }
       }
  |  btype conop btype
       { let loc = combineLocs [getLoc $1, getLoc $3]
         in
           locate loc $
           OpConDecl (unLoc $1) (unLoc $2) defaultFixity (unLoc $3)
       }
  |  con '{' fielddecl_list '}'
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $
           RecConDecl (unLoc $1) defaultFixity (reverse $3)
       }

newconstr :: { L ConDecl }
newconstr :
     con atype
       { let loc = combineLocs [getLoc $1, getLoc $2]
         in
           locate loc $
           ConDecl (unLoc $1) defaultFixity [unLoc $2]
       }
  |  con '{' fielddecl_list '}'
       { let loc = combineLocs [getLoc $1, getLoc $4]
         in
           locate loc $
           RecConDecl (unLoc $1) defaultFixity (reverse $3)
       }
\end{code}

We reverse the field declarations in the @fielddecl@ rule to maintain the
invariant that all field declarations are reversed, which we need for the
@fielddecl_list@ rule.

\begin{code}
-- List built in reverse order since vars is in reverse order
fielddecl :: { [(Var, Type)] }
fielddecl :
    vars '::' type
      {  let  { vars  = map unLoc $1
              ; ty    = unLoc $3
              }
         in
           map (\v -> (v, ty)) vars
      }

-- List built in reverse order
fielddecl_list :: { [(Var, Type)] }
fielddecl_list :
     {- empty -}                   { [] }
  |  fielddecl                     { $1 }
  |  fielddecl_list ',' fielddecl  { $3 ++ $1 }
\end{code}

\begin{code}
deriving :: { [L TyCon] }
deriving :
     {- empty -}                  { [] }
  |  'deriving' qtycls            { [L (getLoc $2) $ TyCon (unLoc $2)] }
  |  'deriving' '(' ')'           { [] }
  |  'deriving' '(' dclasses ')'  { reverse $3 }

-- List built in reverse order
dclasses :: { [L TyCon] }
dclasses :
     qtycls               { [L (getLoc $1) $ TyCon (unLoc $1)] }
  |  dclasses ',' qtycls  { (L (getLoc $3) $ TyCon (unLoc $3)) : $1 }
\end{code}

\begin{code}
rhs :: { L Rhs }
rhs :
    '=' exp wherebinds
      { let loc = combineLocs $ getLoc $1 : getLoc $2 : map getLoc $3
        in
          L loc $
          Rhs [(Nothing, unLoc $2)] (map unLoc $3)
      }
  | gdrhs wherebinds
      { let loc = combineLocs $ map getLoc $1 ++ map getLoc $2
        in
          L loc $
          Rhs (map unLoc $ reverse $1) (map unLoc $2)
      }

wherebinds :: { [L Decl] }
wherebinds :
    {- empty -}    { [] }
  | 'where' decls  { $2 }

-- List built in reverse order
gdrhs :: { [L (Maybe Exp, Exp)] }
gdrhs :
    gd        { [$1] }
  | gdrhs gd  { $2 : $1 }

gd :: { L (Maybe Exp, Exp) }
gd :
    '|' exp '=' exp
      {% do  {  let loc = combineLocs [getLoc $1, getLoc $4]
             ;  return $ L loc (Just (unLoc $2), unLoc $4)
             }
      }
\end{code}

\subsection{Expressions}

\begin{code}
exp :: { L Exp }
exp :
     exp0b '::' type
       {  locate (combineLocs [getLoc $1, getLoc $3]) $
          SigExp (unLoc $1) (unLoc $3)
       }
  |  exp0
       { $1 }
  |  ANTI_EXP
       { locate (getLoc $1) $ AntiExp (getANTI_EXP $1) }

exp0 :: { L Exp }
exp0 :
     exp0a  { $1 }
  |  exp0b  { $1 }

exp0a :: { L Exp }
exp0a :
     exp0b qop exp10a
       {% do  {  let loc = combineLocs [getLoc $1, getLoc $3]
              ;  return $ L loc $
                 opappE (unLoc $1) (unLoc $2) defaultFixity (unLoc $3)
              }
       }
  |  exp10a
      { $1 }

exp0b :: { L Exp }
exp0b :
     exp0b qop exp10b
       {% do  {  let loc = combineLocs [getLoc $1, getLoc $3]
              ;  return $ L loc $
                 opappE (unLoc $1) (unLoc $2) defaultFixity (unLoc $3)
              }
       }
  |  exp10b
       { $1 }

exp10a :: { L Exp }
exp10a :
    '\\' apats '->' exp
      {  let  {  loc  = getLoc $1 <--> getLoc $4
              ;  ps   = reverse (map unLoc $2)
              }
         in
         L loc $
         foldr lamE (unLoc $4) ps
      }
  | 'let' decls 'in' exp
      { let loc = getLoc $1 <--> getLoc $4
        in
          locate loc $
          LetExp (map unLoc $2) (unLoc $4)
      }
  | 'if' exp 'then' exp 'else' exp
      { let loc = getLoc $1 <--> getLoc $6
        in
         locate loc $
         IfExp (unLoc $2) (unLoc $4) (unLoc $6)
      }

exp10b :: { L Exp }
exp10b :
    'case' exp 'of' '{' alts '}'
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          locate loc $
          CaseExp (unLoc $2) (reverse $ map unLoc $5)
      }
  | 'case' exp 'of' '{' alts error
      {% do  {  lookForRightBrace
             ;  let loc = combineLocs [getLoc $1, getLoc $6]
             ;  return $ locate loc $
                CaseExp (unLoc $2) (reverse $ map unLoc $5)
             }
      }
  | 'do' '{' stmts '}'
      {% do  {  lookForRightBrace
             ;  let loc = combineLocs [getLoc $1, getLoc $4]
             ;  return $ locate loc $
                DoExp (reverse $ map unLoc $3)
             }
      }
  | 'do' '{' stmts error
      {% do  {  lookForRightBrace
             ;  let loc = combineLocs (getLoc $1 : map getLoc $3)
             ;  return $ locate loc $
                DoExp (reverse $ map unLoc $3)
             }
      }
  | '-' fexp
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          locate loc $
          NegAppExp (unLoc $2)
      }
  | fexp
      { $1 }

fexp :: { L Exp }
fexp :
    fexp_
      {  case $1 of
           {  [e]  ->  e
           ;  _    ->  let loc = combineLocs (map getLoc $1)
                       in
                         L loc $
                         foldl1' appE (reverse $ map unLoc $1)
           }
      }

-- List built in reverse order
fexp_ :: { [L Exp] }
fexp_ :
    aexp        { [$1] }
  | fexp_ aexp  { $2 : $1 }

aexp :: { L Exp }
aexp :
     '~' aexp
       {  let loc = combineLocs [getLoc $1, getLoc $2]
          in
            locate loc $ IrrefutPatExp (unLoc $2)
       }
  |  qvar '@' aexp
       {  let loc = combineLocs [getLoc $1, getLoc $3]
          in
            locate loc $ AsPatExp (unLoc $1) (unLoc $3)
       }
  |  aexp1
       { $1 }

aexp1 :: { L Exp }
aexp1 :
     aexp1 '{' fbind_list '}'
       {  let  {  loc     = combineLocs [getLoc $1, getLoc $4]
               ;  exp     = unLoc $1
               ;  fields  = reverse $3
               }
          in
            locate loc $
            case destructConExp exp of
              {  Just con  -> RecConExp con fields
              ;  Nothing   -> RecUpdateExp exp fields
              }
       }
  |  aexp2
       { $1 }

aexp2 :: { L Exp }
aexp2 :
     qvar
       { locate (getLoc $1) $ VarExp (unLoc $1) }
  |  gcon
       { locate (getLoc $1) $ ConExp (unLoc $1) }
  |  literal
       { $1 }
  |  '(' exp ')'
       {  let loc = getLoc $1 <--> getLoc $3
          in
            locate loc $ ParExp (unLoc $2)
       }
  |  '(' exps2 ')'
       {  let  {  loc   = getLoc $1 <--> getLoc $3
               ;  exps  = reverse (map unLoc $2)
               ;  conExp  = ConExp (TupleCon (length exps)) (fromLoc loc)
               }
          in
            L loc $
            foldl appE conExp exps
       }
  |  '[' list ']'
       {  let  { loc = combineLocs [getLoc $1, getLoc $3] }
          in
            L loc (unLoc $2)
       }
  |  '(' exp0b qop ')'
       {  locate (combineLocs [getLoc $1, getLoc $4]) $
          LSection (unLoc $2) (unLoc $3)
       }
  |  '(' qop_no_minus exp0 ')'
       {  locate (combineLocs [getLoc $1, getLoc $4]) $
          RSection (unLoc $2) (unLoc $3)
       }
  |  '_'
       {  locate (getLoc $1) WildPatExp }

list :: { L Exp }
list :
     exp
       {  let  {  loc     = getLoc $1
               ;  exps    = [unLoc $1]
               ;  nil     = ConExp (Con builtinNil) (fromLoc loc)
               ;  cons    = ConExp (Con builtinCons) (fromLoc loc)
               }
          in
            L loc $
            foldr (\hd tl -> opappE hd cons consFixity tl) nil exps
       }
  |  lexps
       {  let  {  loc     = combineLocs (map getLoc $1)
               ;  exps    = reverse (map unLoc $1)
               ;  nil     = ConExp (Con builtinNil) (fromLoc loc)
               ;  cons    = ConExp (Con builtinCons) (fromLoc loc)
               }
          in
            L loc $
            foldr (\hd tl -> opappE hd cons consFixity tl) nil exps
       }
  |  exp '..'
       {%  do  {  let loc    = combineLocs [getLoc $1, getLoc $2]
               ;  fromExp    <- checkExp (unLoc $1)
               ;  return $ locate loc $
                  ArithSeqExp fromExp Nothing Nothing
               }
       }
  |  exp ',' exp '..'
       {%  do  {  let loc    = combineLocs [getLoc $1, getLoc $4]
               ;  fromExp    <- checkExp (unLoc $1)
               ;  thenExp    <- checkExp (unLoc $3)
               ;  return $ locate loc $
                  ArithSeqExp fromExp (Just thenExp) Nothing
               }
       }
  |  exp '..' exp
       {%  do  {  let loc    = combineLocs [getLoc $1, getLoc $3]
               ;  fromExp    <- checkExp (unLoc $1)
               ;  toExp      <- checkExp (unLoc $3)
               ;  return $ locate loc $
                  ArithSeqExp fromExp Nothing (Just toExp)
               }
       }
  |  exp ',' exp '..' exp
       {%  do  {  let loc    = combineLocs [getLoc $1, getLoc $5]
               ;  fromExp    <- checkExp (unLoc $1)
               ;  thenExp    <- checkExp (unLoc $3)
               ;  toExp      <- checkExp (unLoc $5)
               ;  return $ locate loc $
                  ArithSeqExp fromExp (Just thenExp) (Just toExp)
               }
       }
  |  exp '|' quals
       {%  do  {  let loc    = combineLocs (getLoc $1 : map getLoc $3)
               ;  exp        <- checkExp (unLoc $1)
               ;  let quals  = reverse $ map unLoc $3
               ;  return $ locate loc $
                  ListCompExp exp quals
               }
       }

-- List built in reverse order
lexps :: { [L Exp] }
lexps :
     lexps ',' exp
       { $3 : $1 }
  |  exp ',' exp
       { [$3, $1] }

-- List built in reverse order
exps1 :: { [L Exp] }
exps1 :
    exp               { [$1] }
  | exps1 ',' exp  { $3 : $1 }

-- List built in reverse order
exps2 :: { [L Exp] }
exps2 :
     exps1 ',' exp  { $3 : $1 }
\end{code}

\begin{code}
fbind :: { (Var, Exp)  }
fbind :
    qvar '=' exp
      {  (unLoc $1, unLoc $3) }

-- List built in reverse order
fbind_list :: { [(Var, Exp)] }
fbind_list :
     {- empty -}           { [] }
  |  fbind                 { [$1] }
  |  fbind_list ',' fbind  { $3 : $1 }

pat :: { L Pat }
pat :
    exp0b
       {% do  {  pat  <- checkPat (unLoc $1)
              ;  return $ L (getLoc $1) pat
              }
       }

apat :: { L Pat }
apat :
     aexp
       {% do  {  pat  <- checkPat (unLoc $1)
              ;  return $ L (getLoc $1) pat
              }
       }

-- List built in reverse order
apats :: { [L Pat] }
apats :
     apat        { [$1] }
  |  apats apat  { $2 : $1 }
\end{code}

\begin{code}
qual :: { L Qual }
qual :
     pat '<-' exp
       {% do  {  let loc = combineLocs [getLoc $1, getLoc $3]
              ;  let pat = unLoc $1
              ;  let exp = unLoc $3
              ;  return $ locate loc $
                 GenQual pat exp
              }
       }
  |  'let' decls
       { error "cannot fully handle statements" }
  |  exp
       { error "cannot fully handle statements" }

-- List built in reverse order
quals :: { [L Qual] }
quals :
     {- empty -}     { [] }
  |  qual            { [ $1] }
  |  quals ',' qual  { $3 : $1 }
\end{code}

\begin{code}
alt :: { L Alt }
alt :
    pat '->' exp wherebinds
      {% do  {  let loc = combineLocs $ getLoc $1 : getLoc $3 : map getLoc $4
             ;  let pat = unLoc $1
             ;  let exp = unLoc $3
             ;  return $ L loc $
                Alt pat [(Nothing, exp)] (map unLoc $4)
             }
      }
  | pat gdpat wherebinds
      {% do  {  let loc = combineLocs $ getLoc $1 : getLoc $2 : map getLoc $3
             ;  let pat = unLoc $1
             ;  return $ L loc $
                Alt pat (unLoc $2) (map unLoc $3)
             }
      }

-- List built in reverse order
alts :: { [L Alt] }
alts :
    alt           { [$1] }
  | alts ';' alt  { $3 : $1 }
  | alts ';' ';'  { $1 }

gdpat :: { L [(Maybe Exp, Exp)] }
gdpat :
    '|' exp0b '->' exp
      {% do  {  let loc = combineLocs [getLoc $1, getLoc $4]
             ;  exp <- checkExp (unLoc $2)
             ;  return $ L loc $
                [(Just exp, unLoc $4)]
             }
      }
  | gdpat '|' exp0b '->' exp
      {% do  {  let loc = combineLocs [getLoc $1, getLoc $5]
             ;  exp <- checkExp (unLoc $3)
             ;  return $ L loc $
                (Just exp, unLoc $5) : unLoc $1
             }
      }
\end{code}

No trailing `{\tt ;}' here because we want to be able to parse {\tt stm}
productions from the top-level without trailing semicolons.

\begin{code}
stmt :: { L Stmt }
stmt :
    exp
      { locate (getLoc $1) $ ExpStmt (unLoc $1) }
  | pat '<-' exp
      {% do  {  let loc = combineLocs [getLoc $1, getLoc $3]
             ;  return $ locate loc $
                (PatStmt (unLoc $1) (unLoc $3))
             }
      }
  | 'let' decls
      { let loc = combineLocs $ getLoc $1 : map getLoc $2
        in
          locate loc $ LetStmt (map unLoc $2)
      }
\end{code}

\begin{code}
-- List built in reverse order
stmts :: { [L Stmt] }
stmts :
    stmt            { [$1] }
  | stmts ';' stmt  { $3 : $1 }
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
    varid           { L (getLoc $1) $ Var $ unLoc $1 }
  | '(' varsym ')'  { L (getLoc $2) $ Var $ unLoc $2 }

qvar :: { L Var }
qvar :
    qvarid           { L (getLoc $1) $ Var $ unLoc $1 }
  | '(' varsym ')'   { L (getLoc $2) $ Var $ unLoc $2 }
  | '(' QVARSYM ')'  { L (getLoc $2) $ Var $ qname (getLoc $2) (getQVARSYM $2) }
  |  ANTI_VAR        { L (getLoc $1) $ AntiVar (getANTI_VAR $1) }
  |  ANTI_ID         { L (getLoc $1) $ AntiVarId (getANTI_ID $1) }

con :: { L Con }
con :
    conid           { L (getLoc $1) $ Con $ unLoc $1 }
  | '(' consym ')'  { L (getLoc $2) $ Con $ unLoc $2 }

qcon :: { L Con }
qcon :
    qconid           { L (getLoc $1) $ Con $ unLoc $1 }
  | '(' gconsym ')'  { $2 }

varop :: { L Var }
varop :
    varsym         { L (getLoc $1) $ Var $ unLoc $1 }
  | '`' varid '`'  { L (getLoc $1) $ Var $ unLoc $2 }

qvarop_no_minus :: { L Var }
qvarop_no_minus :
    qvarsym_no_minus  { L (getLoc $1) $ Var $ unLoc $1 }
  | '`' qvarid '`'    { L (getLoc $1) $ Var $ unLoc $2 }

qvarop :: { L Var }
qvarop :
    qvarsym         { L (getLoc $1) $ Var $ unLoc $1 }
  | '`' qvarid '`'  { L (getLoc $1) $ Var $ unLoc $2 }

conop :: { L Con }
conop :
    consym         { L (getLoc $1) $ Con $ unLoc $1 }
  | '`' conid '`'  { L (getLoc $1) $ Con $ unLoc $2 }

qconop :: { L Con }
qconop :
    gconsym         { $1 }
  | '`' qconid '`'  { L (getLoc $1) $ Con $ unLoc $2 }

op :: { L Name }
op :
    varop  { let L loc (Var op) = $1
             in
               L loc op
           }
  | conop  { let L loc (Con op) = $1
             in
               L loc op
           }

qop_no_minus :: { L Exp }
qop_no_minus :
    qvarop_no_minus  { locate (getLoc $1) $ VarExp (unLoc $1) }
  | qconop           { locate (getLoc $1) $ ConExp (unLoc $1) }

qop :: { L Exp }
qop :
    qvarop  { locate (getLoc $1) $ VarExp (unLoc $1) }
  | qconop  { locate (getLoc $1) $ ConExp (unLoc $1) }

gconsym :: { L Con }
gconsym :
    ':'      { L (getLoc $1) $ Con $ name (getLoc $1) ":" }
  | qconsym  { L (getLoc $1) $ Con $ unLoc $1 }
\end{code}

\begin{code}
{
\end{code}

\subsection{Lexing}

\begin{code}
runLayout :: (MonadParser m) => m ()
runLayout = do
    inp <- getTokens
    case inp of
      [] -> do  tokenStack <- getTokenStack
                layoutStack <- getLayoutStack
                (inp', tokenStack', layoutStack')
                    <- lay (tokenStack, layoutStack)
                setTokens $ filter (not . isWhitespace) inp'
                setTokenStack tokenStack'
                setLayoutStack layoutStack'
                runLayout
      _ -> return ()
    setNeedRightBrace False
  where
    isWhitespace :: L Token -> Bool
    isWhitespace (L _ (Tws _))  = True
    isWhitespace _              = False

lookForRightBrace :: (MonadParser m) => m ()
lookForRightBrace = do
    setNeedRightBrace True
    t <- getLastToken
    ts <- getTokens
    setTokens (t : ts)
    runLayout
    t <- nextToken
    case t of
      L _ Trbrace      -> return ()
      L (Loc pos _) _  -> throwExceptionAt pos $ Expected ["}"]

nextToken :: (MonadParser m) => m (L Token)
nextToken = do
    runLayout
    (t : ts) <- getTokens
    setLastToken t
    setTokens ts
    return t

lexer :: (MonadParser m) => (L Token -> m a) -> m a
lexer cont = do
    tok <- nextToken
    cont tok
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

getANTI_ID  (L _ (Tanti_id s))        = s
getANTI_ID  _                         = internalErr $ text "not an ANTI_ID"

getANTI_INT  (L _ (Tanti_int s))      = s
getANTI_INT  _                        = internalErr $ text "not an ANTI_INT"

getANTI_FLOAT  (L _ (Tanti_float s))  = s
getANTI_FLOAT  _                      = internalErr $ text "not an ANTI_FLOAT"

getANTI_VAR  (L _ (Tanti_var v))      = v
getANTI_VAR  _                        = internalErr $ text "not an ANTI_VAR"

getANTI_TYPE  (L _ (Tanti_type ty))   = ty
getANTI_TYPE  _                       = internalErr $ text "not an ANTI_TYPE"

getANTI_EXP  (L _ (Tanti_exp e))      = e
getANTI_EXP  _                        = internalErr $ text "not an ANTI_EXP"
\end{code}

%if style == code
\begin{code}
}
\end{code}
%endif
