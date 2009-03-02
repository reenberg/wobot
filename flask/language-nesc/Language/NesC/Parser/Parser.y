{
{-# OPTIONS -w #-}

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
-- Module      :  Language.NesC.Parser.Parser
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.NesC.Parser.Parser where

import Control.Monad (forM_)

import Data.Loc
import Control.Monad.ContextException
import Control.Monad.Exception
import Language.NesC.Parser.Exceptions
import Language.NesC.Parser.Lexer
import Language.NesC.Parser.Monad
import qualified Language.NesC.Parser.Tokens as T
import Language.NesC.Pretty
import Language.NesC.Syntax
import qualified Language.NesC.Syntax as C
import Text.PrettyPrint.Mainland
}

%token
 CHAR        { L _ (T.TcharConst _) }
 STRING      { L _ (T.TstringConst _) }
 INT         { L _ (T.TintConst _) }
 LONG        { L _ (T.TlongIntConst _) }
 LONG_LONG   { L _ (T.TlongLongIntConst _) }
 FLOAT       { L _ (T.TfloatConst _) }
 DOUBLE      { L _ (T.TdoubleConst _) }
 LONG_DOUBLE { L _ (T.TlongDoubleConst _) }
 ID          { L _ (T.Tidentifier _) }
 NAMED       { L _ (T.Tnamed _) }

 '('    { L _ T.Tlparen }
 ')'    { L _ T.Trparen }
 '['    { L _ T.Tlbrack }
 ']'    { L _ T.Trbrack }
 '{'    { L _ T.Tlbrace }
 '}'    { L _ T.Trbrace }
 ','    { L _ T.Tcomma }
 ';'    { L _ T.Tsemi }
 ':'    { L _ T.Tcolon }
 '?'    { L _ T.Tquestion }
 '.'    { L _ T.Tdot }
 '->'   { L _ T.Tarrow }
 '...'  { L _ T.Tellipses }

 '+'   { L _ T.Tplus }
 '-'   { L _ T.Tminus }
 '*'   { L _ T.Tstar }
 '/'   { L _ T.Tdiv }
 '%'   { L _ T.Tmod }
 '~'   { L _ T.Tnot }
 '&'   { L _ T.Tand }
 '|'   { L _ T.Tor }
 '^'   { L _ T.Txor }
 '<<'  { L _ T.Tlsh }
 '>>'  { L _ T.Trsh }
 '++'  { L _ T.Tinc }
 '--'  { L _ T.Tdec }

 '!'   { L _ T.Tlnot }
 '&&'  { L _ T.Tland }
 '||'  { L _ T.Tlor }

 '=='  { L _ T.Teq }
 '!='  { L _ T.Tne }
 '<'   { L _ T.Tlt }
 '>'   { L _ T.Tgt }
 '<='  { L _ T.Tle }
 '>='  { L _ T.Tge }

 '='   { L _ T.Tassign }
 '+='  { L _ T.Tadd_assign }
 '-='  { L _ T.Tsub_assign }
 '*='  { L _ T.Tmul_assign }
 '/='  { L _ T.Tdiv_assign }
 '%='  { L _ T.Tmod_assign }
 '<<=' { L _ T.Tlsh_assign }
 '>>=' { L _ T.Trsh_assign }
 '&='  { L _ T.Tand_assign }
 '|='  { L _ T.Tor_assign }
 '^='  { L _ T.Txor_assign }

 'auto'     { L _ T.Tauto }
 'break'    { L _ T.Tbreak }
 'case'     { L _ T.Tcase }
 'char'     { L _ T.Tchar }
 'const'    { L _ T.Tconst }
 'continue' { L _ T.Tcontinue }
 'default'  { L _ T.Tdefault }
 'do'       { L _ T.Tdo }
 'double'   { L _ T.Tdouble }
 'else'     { L _ T.Telse }
 'enum'     { L _ T.Tenum }
 'extern'   { L _ T.Textern }
 'float'    { L _ T.Tfloat }
 'for'      { L _ T.Tfor }
 'goto'     { L _ T.Tgoto }
 'if'       { L _ T.Tif }
 'int'      { L _ T.Tint }
 'long'     { L _ T.Tlong }
 'register' { L _ T.Tregister }
 'return'   { L _ T.Treturn }
 'short'    { L _ T.Tshort }
 'signed'   { L _ T.Tsigned }
 'sizeof'   { L _ T.Tsizeof }
 'static'   { L _ T.Tstatic }
 'struct'   { L _ T.Tstruct }
 'switch'   { L _ T.Tswitch }
 'typedef'  { L _ T.Ttypedef }
 'union'    { L _ T.Tunion }
 'unsigned' { L _ T.Tunsigned }
 'void'     { L _ T.Tvoid }
 'volatile' { L _ T.Tvolatile }
 'while'    { L _ T.Twhile }

 'asm'             { L _ T.Tasm }
 'attribute'       { L _ T.Tattribute }
 'builtin_va_arg'  { L _ T.Tbuiltin_va_arg }
 'builtin_va_list' { L _ T.Tbuiltin_va_list }
 'inline'          { L _ T.Tinline }

 'typename'        { L _ T.Ttypename }

 ANTI_ID          { L _ (T.Tanti_id _) }
 ANTI_INT         { L _ (T.Tanti_int _) }
 ANTI_UINT        { L _ (T.Tanti_uint _) }
 ANTI_LINT        { L _ (T.Tanti_lint _) }
 ANTI_ULINT       { L _ (T.Tanti_ulint _) }
 ANTI_FLOAT       { L _ (T.Tanti_float _) }
 ANTI_DOUBLE      { L _ (T.Tanti_double _) }
 ANTI_LONG_DOUBLE { L _ (T.Tanti_long_double _) }
 ANTI_CHAR        { L _ (T.Tanti_char _) }
 ANTI_STRING      { L _ (T.Tanti_string _) }
 ANTI_EXP         { L _ (T.Tanti_exp _) }
 ANTI_FUNC        { L _ (T.Tanti_func _) }
 ANTI_ARGS        { L _ (T.Tanti_args _) }
 ANTI_DECL        { L _ (T.Tanti_decl _) }
 ANTI_DECLS       { L _ (T.Tanti_decls _) }
 ANTI_SDECL       { L _ (T.Tanti_sdecl _) }
 ANTI_SDECLS      { L _ (T.Tanti_sdecls _) }
 ANTI_ENUM        { L _ (T.Tanti_enum _) }
 ANTI_ENUMS       { L _ (T.Tanti_enums _) }
 ANTI_EDECL       { L _ (T.Tanti_edecl _) }
 ANTI_EDECLS      { L _ (T.Tanti_edecls _) }
 ANTI_STM         { L _ (T.Tanti_stm _) }
 ANTI_STMS        { L _ (T.Tanti_stms _) }
 ANTI_PARAM       { L _ (T.Tanti_param _) }
 ANTI_PARAMS      { L _ (T.Tanti_params _) }
 ANTI_TYPE        { L _ (T.Tanti_type _) }

 'as'             { L _ T.Tas }
 'atomic'         { L _ T.Tatomic }
 'async'          { L _ T.Tasync }
 'call'           { L _ T.Tcall }
 'command'        { L _ T.Tcommand }
 'components'     { L _ T.Tcomponents }
 'configuration'  { L _ T.Tconfiguration }
 'event'          { L _ T.Tevent }
 'generic'        { L _ T.Tgeneric }
 'implementation' { L _ T.Timplementation }
 'includes'       { L _ T.Tincludes }
 'interface'      { L _ T.Tinterface }
 'module'         { L _ T.Tmodule }
 'new'            { L _ T.Tnew }
 'norace'         { L _ T.Tnorace }
 'post'           { L _ T.Tpost }
 'provides'       { L _ T.Tprovides }
 'signal'         { L _ T.Tsignal }
 'task'           { L _ T.Ttask }
 'uses'           { L _ T.Tuses }
 'abstract'       { L _ T.Tabstract }
 'component'      { L _ T.Tcomponent }
 'extends'        { L _ T.Textends }
 '<-'             { L _ T.Tleft_arrow }

 ANTI_USES_PROVIDES      { L _ (T.Tanti_uses_provides _) }
 ANTI_USES_PROVIDES_LIST { L _ (T.Tanti_uses_provides_list _) }
 ANTI_COMPONENTS         { L _ (T.Tanti_components _) }
 ANTI_COMPONENTS_LIST    { L _ (T.Tanti_components_list _) }
 ANTI_CONNECTION         { L _ (T.Tanti_connection _) }
 ANTI_CONNECTIONS        { L _ (T.Tanti_connections _) }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseExp        expression

%name parseEdecl      external_declaration
%name parseDecl       declaration
%name parseStructDecl struct_declaration
%name parseEnum       enumerator

%name parseType       type_declaration
%name parseParam      parameter_declaration
%name parseInit       initializer

%name parseStm        statement

%name parseUnit       translation_unit
%name parseFunc       function_definition

%name parseNesCFile     nesc_file
%name parseUsesProvides uses_provides
%name parseComponents   components
%name parseConnection   connection

%right NAMED
%%

{------------------------------------------------------------------------------
 -
 - Identifiers
 -
 ------------------------------------------------------------------------------}

identifier :: { L Id }
identifier :
    ID
      { L (getLoc $1) $ Id (getID $1) }
  | 'abstract'
      {% do{ nesc <- getNesC
           ; if nesc
             then throwExceptionAt (getLoc $1) (Reserved "abstract")
             else return $ L (getLoc $1) $ Id "abstract"
           }
      }
  | 'extends'
      {% do{ nesc <- getNesC
           ; if nesc
             then throwExceptionAt (getLoc $1) (Reserved "extends")
             else return $ L (getLoc $1) $ Id "extends"
           }
      }
  | ANTI_ID
      { L (getLoc $1) $ AntiId (getANTI_ID $1) }

identifier_or_typename :: { L Id }
identifier_or_typename :
    identifier
      { $1 }
  | NAMED
      { L (getLoc $1) $ Id $ getNAMED $1 }

{------------------------------------------------------------------------------
 -
 - Constants
 -
 ------------------------------------------------------------------------------}

constant :: { L Const }
constant :
    INT
      { L (getLoc $1) $ IntConst (getINT $1) }
  | LONG
      { L (getLoc $1) $ LongIntConst (getLONG $1) }
  | LONG_LONG
      { L (getLoc $1) $ LongLongIntConst (getLONG_LONG $1) }
  | FLOAT
      { L (getLoc $1) $ FloatConst (getFLOAT $1) }
  | DOUBLE
      { L (getLoc $1) $ DoubleConst (getDOUBLE $1) }
  | LONG_DOUBLE
      { L (getLoc $1) $ LongDoubleConst (getLONG_DOUBLE $1) }
  | CHAR
      { L (getLoc $1) $ CharConst (getCHAR $1) }
  | ANTI_INT
      { L (getLoc $1) $ AntiInt (getANTI_INT $1) }
  | ANTI_UINT
      { L (getLoc $1) $ AntiUInt (getANTI_UINT $1) }
  | ANTI_LINT
      { L (getLoc $1) $ AntiLInt (getANTI_LINT $1) }
  | ANTI_ULINT
      { L (getLoc $1) $ AntiULInt (getANTI_ULINT $1) }
  | ANTI_FLOAT
      { L (getLoc $1) $ AntiFloat (getANTI_FLOAT $1) }
  | ANTI_DOUBLE
      { L (getLoc $1) $ AntiDouble (getANTI_DOUBLE $1) }
  | ANTI_LONG_DOUBLE
      { L (getLoc $1) $ AntiLongDouble (getANTI_LONG_DOUBLE $1) }
  | ANTI_CHAR
      { L (getLoc $1) $ AntiChar (getANTI_CHAR $1) }
  | ANTI_STRING
      { L (getLoc $1) $ AntiString (getANTI_STRING $1) }


{------------------------------------------------------------------------------
 -
 - Expressions
 -
 ------------------------------------------------------------------------------}

primary_expression :: { L Exp }
primary_expression :
    identifier
      { let loc = getLoc $1
        in
          locate loc $ Var (unLoc $1) }
  | constant
      { let loc = getLoc $1
        in
          locate loc $ Const (unLoc $1)
      }
  | string_constant
      { let loc = combineLocs (map getLoc $1)
        in
          locate loc $ Const $ StringConst $ concat $ reverse $ map unLoc $1
      }
  | '(' expression ')'
      { $2 }
  | '(' expression error
      {% do{ let loc = getLoc $1 <--> getLoc $2
           ; throwExceptionAt loc $ Unclosed "("
           }
      }
  | '(' compound_statement ')'
      { let { loc = getLoc $1 <--> getLoc $3
            ; (initGroups, stms) = unLoc $2
            }
        in
          locate loc $ StmExpr (map unLoc initGroups) (map unLoc stms)
      }
  | ANTI_EXP
      { locate (getLoc $1) (AntiExp (getANTI_EXP $1)) }

{- Extension: GCC -}

-- List built in reverse order
string_constant :: { [L String] }
string_constant :
    STRING
      { [L (getLoc $1) (getSTRING $1)] }
  | string_constant STRING
      { L (getLoc $2) (getSTRING $2) : $1 }

postfix_expression :: { L Exp }
postfix_expression :
    primary_expression
      { $1 }
  | postfix_expression '[' error
      {% throwExceptionAt (getLoc $2) (Unclosed "[") }
  | postfix_expression '[' expression ']'
      { let loc = getLoc $1 <--> getLoc $4
        in
          locate loc $ Index (unLoc $1) (unLoc $3)
      }
  | postfix_expression '(' error
      {% throwExceptionAt (getLoc $2) (Unclosed "(") }
  | postfix_expression '(' ')'
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ FnCall NormalCall (unLoc $1) [] []
      }
  | postfix_expression '(' argument_expression_list error
      {% do{ let loc = getLoc $2 <--> getLoc (last $3)
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | postfix_expression '(' argument_expression_list ')'
      { let loc = getLoc $1 <--> getLoc $4
        in
          locate loc $ FnCall NormalCall
                              (unLoc $1)
                              []
                              (reverse $ map unLoc $3)
      }
  | postfix_expression '.' identifier_or_typename
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ Member (unLoc $1) (unLoc $3)
      }
  | postfix_expression '->' identifier_or_typename
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ PtrMember (unLoc $1) (unLoc $3)
      }
  | postfix_expression '++'
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ PostInc (unLoc $1)
      }
  | postfix_expression '--'
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ PostDec (unLoc $1)
      }
  {- Extension: GCC -}
  | 'builtin_va_arg' '(' assignment_expression ',' type_declaration ')'
      { let loc = getLoc $1 <--> getLoc $6
        in
          locate loc $ BuiltinVaArg (unLoc $3) (unLoc $5)
      }
  {- Extension: NesC -}
  | call_kind '(' error
      {% throwExceptionAt (getLoc (fst $1)) (Unclosed "(") }
  | call_kind '(' ')'
      { let { (kind, f) = $1
            ; loc = kind <--> getLoc $3
            }
        in
          locate loc $ FnCall (unLoc kind) (unLoc f) [] []
      }
  | call_kind '(' argument_expression_list error
      {% do{ let loc = combineLocs (getLoc (fst $1) : map getLoc $3)
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | call_kind '(' argument_expression_list ')'
      { let { (kind, f) = $1
            ; loc = combineLocs [getLoc kind, getLoc $4]
            }
        in
          locate loc $ FnCall (unLoc kind)
                              (unLoc f)
                              []
                              (reverse $ map unLoc $3)
      }
  | call_kind  '[' error
      {% let { (kind, f) = $1
             ; loc = combineLocs [getLoc kind, getLoc $2]
             }
         in
           throwExceptionAt loc (Unclosed "[")
      }
  | call_kind '[' argument_expression_list error
      {% let { (kind, f) = $1
             ; loc = combineLocs (getLoc kind : map getLoc $3)
             }
         in
           throwExceptionAt loc (Unclosed "[")
      }
  | call_kind '[' argument_expression_list ']' '(' error
      {% do{ let loc = combineLocs [getLoc (fst $1), getLoc $5]
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | call_kind '[' argument_expression_list ']' '(' ')'
      { let { (kind, f) = $1
            ; loc = combineLocs [getLoc kind, getLoc $6]
            }
        in
          locate loc $ FnCall (unLoc kind) (unLoc f)
                              (reverse $ map unLoc $3)
                              []
      }
  | call_kind '[' argument_expression_list ']' '(' argument_expression_list error
      {% do{ let loc = combineLocs (getLoc (fst $1) : map getLoc $6)
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | call_kind '[' argument_expression_list ']' '(' argument_expression_list ')'
      { let { (kind, f) = $1
            ; loc = combineLocs [getLoc kind, getLoc $7]
            }
        in
          locate loc $ FnCall (unLoc kind) (unLoc f)
                              (reverse $ map unLoc $3)
                              (reverse $ map unLoc $6)
      }

{- Extension: NesC -}
call_kind :: { (L CallKind, L Exp) }
call_kind
  : 'call' identifier
      { (L (getLoc $1) Call, locate (getLoc $2) (Var $ unLoc $2)) }
  | 'call' identifier '.' identifier
      { let loc = combineLocs [getLoc $2, getLoc $4]
        in
          (L (getLoc $1) Call, locate (getLoc $2)
                               $ InterfaceMember (unLoc $2) (unLoc $4))
      }
  | 'signal' identifier
      { (L (getLoc $1) Signal, locate (getLoc $2) (Var $ unLoc $2)) }
  | 'signal' identifier '.' identifier
      { let loc = combineLocs [getLoc $2, getLoc $4]
        in
          (L (getLoc $1) Signal, locate (getLoc $2)
                                 $ InterfaceMember (unLoc $2) (unLoc $4))
      }
  | 'post' identifier
      { (L (getLoc $1) Post, locate (getLoc $2) (Var $ unLoc $2)) }
  | 'post' identifier '.' identifier
      { let loc = combineLocs [getLoc $2, getLoc $4]
        in
          (L (getLoc $1) Post, locate (getLoc $2)
                               $ InterfaceMember (unLoc $2) (unLoc $4))
      }

-- List built in reverse order
argument_expression_list :: { [L Exp] }
argument_expression_list :
    assignment_expression
      { [$1] }
  | ANTI_ARGS
      { [locate (getLoc $1) $ AntiArgs (getANTI_ARGS $1)] }
  | argument_expression_list ',' assignment_expression
      { $3 : $1}
  | argument_expression_list ',' ANTI_ARGS
      { (locate (getLoc $2) $ AntiArgs (getANTI_ARGS $3)) : $1 }

unary_expression :: { L Exp }
unary_expression :
    postfix_expression
      { $1 }
  | '++' unary_expression
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ PreInc (unLoc $2)
      }
  | '--' unary_expression
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ PreDec (unLoc $2)
      }
  | unary_operator cast_expression
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ UnOp (unLoc $1) (unLoc $2)
      }
  | 'sizeof' unary_expression
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ SizeofExp (unLoc $2)
      }
  | 'sizeof' '(' type_name ')'
      { let loc = getLoc $1 <--> getLoc $4
        in
          locate loc $ SizeofType (unLoc $3)
      }
  | 'sizeof' '(' type_name error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "(")
           }
      }

unary_operator :: { L UnOp }
unary_operator :
    '&' { L (getLoc $1) $ AddrOf }
  | '*' { L (getLoc $1) $ Deref }
  | '+' { L (getLoc $1) $ Positive }
  | '-' { L (getLoc $1) $ Negate }
  | '~' { L (getLoc $1) $ Not }
  | '!' { L (getLoc $1) $ Lnot }

cast_expression :: { L Exp }
cast_expression :
    unary_expression
      { $1 }
  | '(' type_name ')' cast_expression
      { let loc = getLoc $1 <--> getLoc $4
        in
          locate loc $ Cast (unLoc $2) (unLoc $4)
      }
  | '(' type_name error
      {% do{ let loc = getLoc $1 <--> getLoc $2
           ; throwExceptionAt loc (Unclosed "(")
           }
      }

multiplicative_expression :: { L Exp }
multiplicative_expression :
    cast_expression
      { $1 }
  | multiplicative_expression '*' cast_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Mul (unLoc $1) (unLoc $3)
      }
  | multiplicative_expression '/' cast_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Div (unLoc $1) (unLoc $3)
      }
  | multiplicative_expression '%' cast_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Mod (unLoc $1) (unLoc $3)
      }

additive_expression :: { L Exp }
additive_expression :
    multiplicative_expression
      { $1 }
  | additive_expression '+' multiplicative_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Add (unLoc $1) (unLoc $3)
      }
  | additive_expression '-' multiplicative_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Sub (unLoc $1) (unLoc $3)
      }

shift_expression :: { L Exp }
shift_expression :
    additive_expression
      { $1 }
  | shift_expression '<<' additive_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Lsh (unLoc $1) (unLoc $3)
      }
  | shift_expression '>>' additive_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Rsh (unLoc $1) (unLoc $3)
      }

relational_expression :: { L Exp }
relational_expression :
    shift_expression
      { $1 }
  | relational_expression '<' shift_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Lt (unLoc $1) (unLoc $3)
      }
  | relational_expression '>' shift_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Gt (unLoc $1) (unLoc $3)
      }
  | relational_expression '<=' shift_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Le (unLoc $1) (unLoc $3)
      }
  | relational_expression '>=' shift_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Ge (unLoc $1) (unLoc $3)
      }

equality_expression :: { L Exp }
equality_expression :
    relational_expression
      { $1 }
  | equality_expression '==' relational_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Eq (unLoc $1) (unLoc $3)
      }
  | equality_expression '!=' relational_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Ne (unLoc $1) (unLoc $3)
      }

and_expression :: { L Exp }
and_expression :
    equality_expression
      { $1 }
  | and_expression '&' equality_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp And (unLoc $1) (unLoc $3)
      }

exclusive_or_expression :: { L Exp }
exclusive_or_expression :
    and_expression
      { $1 }
  | exclusive_or_expression '^' and_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Xor (unLoc $1) (unLoc $3)
      }

inclusive_or_expression :: { L Exp }
inclusive_or_expression :
    exclusive_or_expression
      { $1 }
  | inclusive_or_expression '|' exclusive_or_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Or (unLoc $1) (unLoc $3)
      }

logical_and_expression :: { L Exp }
logical_and_expression :
    inclusive_or_expression
      { $1 }
  | logical_and_expression '&&' inclusive_or_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Land (unLoc $1) (unLoc $3)
      }

logical_or_expression :: { L Exp }
logical_or_expression :
    logical_and_expression
      { $1 }
  | logical_or_expression '||' logical_and_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ BinOp Lor (unLoc $1) (unLoc $3)
      }

conditional_expression :: { L Exp }
conditional_expression :
    logical_or_expression
      { $1 }
  | logical_or_expression '?' expression ':' conditional_expression
      { let loc = getLoc $1 <--> getLoc $5
        in
          locate loc $ Cond (unLoc $1) (unLoc $3) (unLoc $5) }

assignment_expression :: { L Exp }
assignment_expression :
    conditional_expression
      { $1 }
  | unary_expression assignment_operator assignment_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ Assign (unLoc $2) (unLoc $1) (unLoc $3)
      }

assignment_operator :: { L AssignOp }
assignment_operator :
    '='   { L (getLoc $1) JustAssign }
  | '*='  { L (getLoc $1) MulAssign }
  | '/='  { L (getLoc $1) DivAssign }
  | '%='  { L (getLoc $1) ModAssign }
  | '+='  { L (getLoc $1) AddAssign }
  | '-='  { L (getLoc $1) SubAssign }
  | '<<=' { L (getLoc $1) LshAssign }
  | '>>=' { L (getLoc $1) RshAssign }
  | '&='  { L (getLoc $1) AndAssign }
  | '^='  { L (getLoc $1) XorAssign }
  | '|='  { L (getLoc $1) OrAssign }

expression :: { L Exp }
expression :
    assignment_expression
      { $1 }
  | expression ',' assignment_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ Seq (unLoc $1) (unLoc $3)
      }

constant_expression :: { L Exp }
constant_expression :
  conditional_expression { $1 }

{------------------------------------------------------------------------------
 -
 - Declarations
 -
 ------------------------------------------------------------------------------}

{-
-- XXX: This is an awful hack to get around problems with the interaction
-- between lexer feedback and the one-token lookahead that happy does. If we
-- encounter a typedef and the next token is the newly typedef'd type, we get an
-- error if we include the terminal semicolon directly in the productions for
-- declaration. By splitting the semicolon out the lookahead token is guaranteed
-- not to be a typedef use :)
-}

declaration :: { L InitGroup }
declaration :
    declaration_ ';' { $1 }

declaration_ :: { L InitGroup }
declaration_ :
    declaration_specifiers
      {% do{ let (declSpec, decl) = unLoc $1
           ; let loc              = getLoc $1
           ; initgroup <- withLocContext loc empty $
                          checkInitGroup (InitGroup declSpec [] [])
           ; return $ L loc initgroup
           }
      }
  | declaration_specifiers init_declarator_list
      {% do{ let (declSpec, decl) = unLoc $1
           ; let inits            = reverse $ map unLoc $2
           ; let loc              = getLoc $1 <--> getLoc (last $2)
           ; let inits' = map (\(Init id initDecl maybe_exp attrs) ->
                                   let newDecl = composeDecls initDecl decl
                                   in
                                     Init id newDecl maybe_exp attrs)
                              inits
           ; initgroup <- withLocContext loc empty $
                          checkInitGroup (InitGroup declSpec [] inits')
           ; return $ L loc initgroup
           }
      }
  | declaration_specifiers attributes init_declarator_list
      {% do{ let (declSpec, decl) = unLoc $1
           ; let attrs            = reverse $ map unLoc $2
           ; let inits            = reverse $ map unLoc $3
           ; let loc              = getLoc $1 <--> getLoc (last $3)
           ; let inits' = map (\(Init id initDecl maybe_exp attrs) ->
                                   let newDecl = composeDecls initDecl decl
                                   in
                                     Init id newDecl maybe_exp attrs)
                              inits
           ; initgroup <- withLocContext loc empty $
                          checkInitGroup (InitGroup declSpec attrs inits')
           ; return $ L loc initgroup
           }
      }
  | declaration_specifiers error
      {% throwExceptionAt ((locEnd . getLoc) $1) (Expected ["';'"]) }
  | ANTI_DECL
      { L (getLoc $1) (AntiDecl (getANTI_DECL $1)) }

declaration_specifiers :: { L (DeclSpec, Decl) }
declaration_specifiers :
    ANTI_TYPE
      { let v = getANTI_TYPE $1
        in
          L (getLoc $1) $ (AntiTypeDeclSpec [] [] v, AntiTypeDecl v)
      }
  | storage_qualifier_specifiers ANTI_TYPE
      { let { storage   = mkStorage (map unLoc $1)
            ; typeQuals = mkTypeQuals (map unLoc $1)
            ; v         = getANTI_TYPE $2
            ; loc       = $2 <--> $1
            }
        in
          L loc $ (AntiTypeDeclSpec storage typeQuals v, AntiTypeDecl v)
      }
  | nontypedef_declaration_specifiers
      { $1 }
  | typedef_declaration_specifiers
      { $1 }

nontypedef_declaration_specifiers :: { L (DeclSpec, Decl) }
nontypedef_declaration_specifiers :
    storage_qualifier_specifiers %prec NAMED
      {% do{ let loc = getLoc $1
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (map unLoc $1)
           ; return $ L loc (declSpec, DeclRoot)
           }
      }
  | type_specifier
      {% do{ let loc = getLoc $1
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec [unLoc $1]
           ; return $ L loc (declSpec, DeclRoot)
           }
      }
  | type_specifier declaration_specifiers_
      {% do{ let loc = getLoc $1 <--> getLoc $2
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (unLoc $1 : map unLoc $2)
           ; return $ L loc (declSpec, DeclRoot)
           }
      }
  | storage_qualifier_specifiers type_specifier
      {% do{ let loc = getLoc $1 <--> getLoc $2
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (map unLoc $1 ++ [unLoc $2])
           ; return $ L loc (declSpec, DeclRoot)
           }
      }
  | storage_qualifier_specifiers type_specifier declaration_specifiers_
      {% do{ let loc = getLoc $1 <--> getLoc $3
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (map unLoc $1 ++ unLoc $2 : map unLoc $3)
           ; return $ L loc (declSpec, DeclRoot)
           }
      }

typedef_declaration_specifiers :: { L (DeclSpec, Decl) }
typedef_declaration_specifiers :
    typedef_name
      {% do{ let loc = getLoc $1
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec [unLoc $1]
           ; return $ L loc (declSpec, DeclRoot)
           }
      }
  | typedef_name storage_qualifier_specifiers
      {% do{ let loc = getLoc $1 <--> getLoc $2
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (unLoc $1 : map unLoc $2)
           ; return $ L loc (declSpec, DeclRoot)
           }
      }
  | storage_qualifier_specifiers typedef_name
      {% do{ let loc = getLoc $1 <--> getLoc $2
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (map unLoc $1 ++ [unLoc $2])
           ; return $ L loc (declSpec, DeclRoot)
           }
      }
  | storage_qualifier_specifiers typedef_name storage_qualifier_specifiers
      {% do{ let loc = getLoc $1 <--> getLoc $3
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (map unLoc $1 ++ unLoc $2 : map unLoc $3)
           ; return $ L loc (declSpec, DeclRoot)
           }
      }

declaration_specifiers_ :: { [L TySpec] }
declaration_specifiers_ :
    storage_class_specifier %prec NAMED
      { [$1] }
  | storage_class_specifier declaration_specifiers_
      { $1 : $2 }
  | type_specifier %prec NAMED
      { [$1] }
  | type_specifier declaration_specifiers_
      { $1 : $2 }
  | type_qualifier %prec NAMED
      { [$1] }
  | type_qualifier declaration_specifiers_
      { $1 : $2 }

-- | This production allows us to add storage class specifiers and type
-- qualifiers to an anti-quoted type.

storage_qualifier_specifiers :: { [L TySpec] }
storage_qualifier_specifiers :
    storage_class_specifier %prec NAMED
      { [$1] }
  | storage_class_specifier storage_qualifier_specifiers
      { $1 : $2 }
  | type_qualifier %prec NAMED
      { [$1] }
  | type_qualifier storage_qualifier_specifiers
      { $1 : $2 }

-- List built in reverse order
init_declarator_list :: { [L Init] }
init_declarator_list :
    init_declarator
      { [$1] }
  | init_declarator_list ',' init_declarator
      { $3 : $1 }

init_declarator :: { L Init }
init_declarator :
    declarator
      { let { (id, declToDecl) = $1
            ; L declLoc decl   = declToDecl (L (getLoc id) DeclRoot)
            }
        in
          L declLoc $ Init (unLoc id) decl Nothing []
      }
  | declarator attributes
      { let { (id, declToDecl) = $1
            ; L declLoc decl   = declToDecl (L (getLoc id) DeclRoot)
            ; loc              = declLoc <--> getLoc (last $2)
            }
        in
          L loc $ Init (unLoc id) decl Nothing (map unLoc $2)
      }
  | declarator '=' initializer
      { let { (id, declToDecl) = $1
            ; L declLoc decl   = declToDecl (L (getLoc id) DeclRoot)
            ; loc              = declLoc <--> getLoc $3
            }
        in
          L loc $ Init (unLoc id) decl (Just $ unLoc $3) []
      }
  | declarator '=' attributes initializer
      { let { (id, declToDecl) = $1
            ; L declLoc decl   = declToDecl (L (getLoc id) DeclRoot)
            ; loc              = declLoc <--> getLoc $4
            }
        in
          L loc $ Init (unLoc id) decl (Just $ unLoc $4) (map unLoc $3)
      }
  | declarator error
      {% do{ let (id, declToDecl) = $1
           ; let decl             = declToDecl (L (getLoc id) DeclRoot)
           ; throwExceptionAt ((locEnd . getLoc) decl) (Expected ["'='"])
           }
      }

storage_class_specifier :: { L TySpec }
storage_class_specifier :
    'auto'      { L (getLoc $1) $ TSauto }
  | 'register'  { L (getLoc $1) $ TSregister }
  | 'static'    { L (getLoc $1) $ TSstatic }
  | 'extern'    { L (getLoc $1) $ TSextern }
  | 'typedef'   { L (getLoc $1) $ TStypedef }

  {- Extension: NesC -}
  | 'async'     { L (getLoc $1) $ TSasync }
  | 'command'   { L (getLoc $1) $ TScommand }
  | 'default'   { L (getLoc $1) $ TSdefault }
  | 'event'     { L (getLoc $1) $ TSevent }
  | 'norace'    { L (getLoc $1) $ TSnorace }
  | 'task'      { L (getLoc $1) $ TStask }

type_specifier :: { L TySpec }
type_specifier :
    'void'                    { L (getLoc $1) $ TSvoid }
  | 'char'                    { L (getLoc $1) $ TSchar }
  | 'short'                   { L (getLoc $1) $ TSshort }
  | 'int'                     { L (getLoc $1) $ TSint }
  | 'long'                    { L (getLoc $1) $ TSlong }
  | 'float'                   { L (getLoc $1) $ TSfloat }
  | 'double'                  { L (getLoc $1) $ TSdouble }
  | 'signed'                  { L (getLoc $1) $ TSsigned }
  | 'unsigned'                { L (getLoc $1) $ TSunsigned }
  | struct_or_union_specifier { $1 }
  | enum_specifier            { $1 }

  {- Extension: GCC -}
  | 'builtin_va_list'         { L (getLoc $1) $ TSva_list }

struct_or_union_specifier :: { L TySpec }
struct_or_union_specifier :
    struct_or_union identifier_or_typename
      { let loc = getLoc $1 <--> getLoc $2
        in
          L loc $ (unLoc $1) (Just $ unLoc $2)
                             Nothing []
      }
  | struct_or_union attributes identifier_or_typename
      { let loc = getLoc $1 <--> getLoc $3
        in
          L loc $ (unLoc $1) (Just $ unLoc $3)
                             Nothing (map unLoc $2)
      }
  | struct_or_union '{' struct_declaration_list '}'
      { let loc = getLoc $1 <--> getLoc $4
        in
          L loc $ (unLoc $1) Nothing
                             (Just $ reverse $ map unLoc $3) []
      }
  | struct_or_union '{' struct_declaration_list error
      {% do{ let loc = getLoc $1 <--> getLoc (last $3)
           ; throwExceptionAt loc (Unclosed "{")
           }
      }
  | struct_or_union identifier_or_typename '{' struct_declaration_list '}'
      { let loc = getLoc $1 <--> getLoc $5
        in
          L loc $ (unLoc $1) (Just $ unLoc $2)
                             (Just $ reverse $ map unLoc $4) []
      }
  | struct_or_union identifier_or_typename '{' struct_declaration_list error
      {% do{ let loc = getLoc $1 <--> getLoc (last $4)
           ; throwExceptionAt loc (Unclosed "{")
           }
      }
  | struct_or_union attributes identifier_or_typename '{' struct_declaration_list '}'
      { let loc = getLoc $1 <--> getLoc $6
        in
          L loc $ (unLoc $1) (Just $ unLoc $3)
                             (Just $ reverse $ map unLoc $5) (map unLoc $2)
      }
  | struct_or_union attributes identifier_or_typename '{' struct_declaration_list error
      {% do{ let loc = getLoc $4 <--> getLoc (last $5)
           ; throwExceptionAt loc (Unclosed "{")
           }
      }

struct_or_union :: { L (Maybe Id -> Maybe [FieldGroup] -> [Attr] -> TySpec) }
struct_or_union :
    'struct' { L (getLoc $1) $ TSstruct }
  | 'union'  { L (getLoc $1) $ TSunion }

-- List built in reverse order
struct_declaration_list :: { [L FieldGroup] }
struct_declaration_list :
    struct_declaration
      { [$1] }
  | ANTI_SDECLS
      { [L (getLoc $1) $ AntiSdecls (getANTI_SDECLS $1)] }
  | struct_declaration_list struct_declaration
      { $2 : $1 }
  | struct_declaration_list ANTI_SDECLS
      { (L (getLoc $2) $ AntiSdecls (getANTI_SDECLS $2)) : $1 }

struct_declaration :: { L FieldGroup }
struct_declaration :
    specifier_qualifier_list struct_declarator_list ';'
      {%  do{ let loc = getLoc $3 <--> getLoc (last $1)
            ; declSpec <- withLocContext loc empty $
                          mkDeclSpec (map unLoc $1)
            ; return $ L loc $ FieldGroup declSpec (reverse $ map unLoc $2)
            }
      }
  | ANTI_TYPE identifier_or_typename ';'
      {%  do{ let v        = getANTI_TYPE $1
            ; let declSpec = AntiTypeDeclSpec [] [] v
            ; let decl     = AntiTypeDecl v
            ; let loc      = getLoc $1 <--> getLoc $3
            ; let field    = Field (Just $ unLoc $2) (Just decl) Nothing
            ; return $ L loc $ FieldGroup declSpec [field]
            }
      }
  | ANTI_SDECL
      { L (getLoc $1) (AntiSdecl (getANTI_SDECL $1)) }

specifier_qualifier_list :: { [L TySpec] }
specifier_qualifier_list :
    type_specifier specifier_qualifier_list_
      { $1 : $2 }
  | type_qualifier_list type_specifier specifier_qualifier_list_
      { $1 ++ [$2] ++ $3 }
  | typedef_name
      { [$1] }
  | typedef_name type_qualifier_list
      { $1 : $2 }
  | type_qualifier_list typedef_name
      { $1 ++ [$2] }
  | type_qualifier_list typedef_name type_qualifier_list
      { $1 ++ [$2] ++ $3 }

specifier_qualifier_list_ :: { [L TySpec] }
specifier_qualifier_list_ :
    {- empty -} %prec NAMED
      { [] }
  | type_specifier %prec NAMED
      { [$1] }
  | type_specifier specifier_qualifier_list
      { $1 : $2 }
  | type_qualifier %prec NAMED
      { [$1] }
  | type_qualifier specifier_qualifier_list
      { $1 : $2 }

-- List built in reverse order
struct_declarator_list :: { [L Field] }
struct_declarator_list :
    struct_declarator                            { [$1] }
  | struct_declarator_list ',' struct_declarator { $3 : $1 }

struct_declarator :: { L Field }
struct_declarator :
    declarator
        { let {(id, declToDecl) = $1
              ; L loc decl      = declToDecl (L (getLoc id) DeclRoot)
              }
          in
            L loc $ Field (Just $ unLoc id) (Just decl) Nothing
        }
  | declarator attributes
        { let {(id, declToDecl) = $1
              ; L loc decl      = declToDecl (L (getLoc id) DeclRoot)
              }
          in
            L loc $ Field (Just $ unLoc id) (Just decl) Nothing
        }
  | ':' constant_expression
        { L (getLoc $2) $ Field Nothing Nothing (Just $ unLoc $2) }
  | declarator ':' constant_expression
        { let {(id, declToDecl) = $1
              ; L declLoc decl  = declToDecl (L (getLoc id) DeclRoot)
              ; loc             = declLoc <--> getLoc $3
              }
          in
            L loc $ Field (Just $ unLoc id) (Just decl) (Just $ unLoc $3)
        }

enum_specifier :: { L TySpec }
enum_specifier :
    'enum' identifier_or_typename
      { let loc = getLoc $1 <--> getLoc $2
        in
          L loc $ TSenum (Just (unLoc $2)) [] []
      }
  | 'enum' attributes identifier_or_typename
      { let loc = getLoc $1 <--> getLoc $3
        in
          L loc $ TSenum (Just (unLoc $3)) [] (map unLoc $2)
      }
  | 'enum' '{' enumerator_list '}'
      { let loc = getLoc $1 <--> getLoc $4
        in
          L loc $ TSenum Nothing (reverse $ map unLoc $3) []
      }
  | 'enum' identifier_or_typename '{' enumerator_list '}'
      { let loc = getLoc $1 <--> getLoc $5
        in
          L loc $ TSenum (Just (unLoc $2)) (reverse $ map unLoc $4) []
      }

-- List built in reverse order
enumerator_list :: { [L CEnum] }
enumerator_list :
    enumerator
      { [$1] }
  | ANTI_ENUMS
      { [L (getLoc $1) $ AntiEnums (getANTI_ENUMS $1)] }
  | enumerator_list ','
      { $1 }
  | enumerator_list ',' enumerator
      { $3 : $1 }
  | enumerator_list ',' ANTI_ENUMS
      { (L (getLoc $2) $ AntiEnums (getANTI_ENUMS $3)) : $1 }

enumerator :: { L CEnum }
enumerator:
    identifier
      { L (getLoc $1) $ CEnum (unLoc $1) Nothing}
  | identifier '=' constant_expression
      { let loc = getLoc $1 <--> getLoc $3
        in
          L loc $ CEnum (unLoc $1) (Just $ unLoc $3)
      }
  | ANTI_ENUM
      { L (getLoc $1) (AntiEnum (getANTI_ENUM $1)) }

type_qualifier :: { L TySpec }
type_qualifier :
    'const'    { L (getLoc $1) $ TSconst }
  | 'volatile' { L (getLoc $1) $ TSvolatile }
  {- Extension: GCC -}
  | 'inline'   { L (getLoc $1) $ TSinline }

declarator :: { (L Id, L Decl -> L Decl) }
declarator :
    direct_declarator
      { $1 }
  | pointer direct_declarator
      { let (id, dirDecl) = $2
        in
          (id, dirDecl . $1)
      }

direct_declarator :: { (L Id, L Decl -> L Decl) }
direct_declarator :
    identifier_or_typename
      { ($1, id) }
  | identifier_or_typename '.' identifier_or_typename
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          (L loc (InterfaceId (unLoc $1) (unLoc $3)), id)
      }
  | '(' declarator ')'
      { let { (id, declToDecl) = $2
            ; loc              = getLoc $1 <--> getLoc $3
            }
        in
          (id, \d -> let L _ decl = declToDecl d
                     in
                       L loc decl)
      }
  | '(' declarator error
      {% throwExceptionAt (getLoc $1) (Unclosed "(") }
  | direct_declarator '[' ']'
      { let (id, declToDecl) = $1
        in
          (id, declToDecl .
                 (\(L loc decl) ->
                      L (getLoc id <--> getLoc $3)
                        (mkArray Nothing decl)))
      }
  | direct_declarator '[' error
      {% throwExceptionAt (getLoc $2) (Unclosed "[") }
  | direct_declarator '[' constant_expression ']'
      { let (id, declToDecl) = $1
        in
          (id, declToDecl .
                 (\(L loc decl) ->
                      L (getLoc id <--> getLoc $4)
                        (mkArray (Just $ unLoc $3) decl)))
      }
  | direct_declarator '[' constant_expression error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "[")
           }
      }
  | direct_declarator '(' parameter_type_list ')'
      { let (id, declToDecl) = $1
        in
          (id, declToDecl .
                 (\(L loc decl) ->
                      L (getLoc id <--> getLoc $4)
                        (mkProto (unLoc $3) decl)))
      }
  | direct_declarator '(' parameter_type_list error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | direct_declarator '(' identifier_list ')'
      { let (id, declToDecl) = $1
        in
          (id, declToDecl .
                 (\(L loc decl) ->
                      L (getLoc id <--> getLoc $4)
                        (mkOldProto (reverse $ map unLoc $3) decl)))
      }
  | direct_declarator '(' identifier_list error
      {% do{ let loc = getLoc $2 <--> getLoc (last $3)
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | direct_declarator '(' ')'
      { let (id, declToDecl) = $1
        in
          (id, declToDecl .
                 (\(L loc decl) ->
                      L (getLoc id <--> getLoc $3)
                        (mkOldProto [] decl)))
      }
  | direct_declarator '(' error
      {% throwExceptionAt (getLoc $2) (Unclosed "(") }

pointer :: { L Decl -> L Decl }
pointer :
    '*'
      { \(L loc decl) -> L (getLoc $1 <--> loc) (mkPtr [] decl) }
  | '*' type_qualifier_list
      { \(L loc decl) -> L (getLoc $1 <--> getLoc (last $2) <--> loc)
                           (mkPtr (reverse $ map unLoc $2) decl)
      }
  | '*' pointer
      { \(L loc decl) ->
            let (L loc' decl') = $2 (L (getLoc $1) (mkPtr [] decl))
            in
              L (getLoc $1 <--> loc <--> loc') decl'
      }
  | '*' type_qualifier_list pointer
      { \(L loc decl) ->
            let { quals          = reverse $ map unLoc $2
                ; (L loc' decl') = $3 (L (getLoc $1) (mkPtr quals decl))
                }
            in
              L (getLoc $1 <--> loc <--> loc') decl'
      }

-- List built in reverse order
type_qualifier_list :: { [L TySpec] }
type_qualifier_list :
    type_qualifier                     { [$1] }
  | type_qualifier_list type_qualifier { $2 : $1 }

parameter_type_list :: { L Params }
parameter_type_list :
    parameter_list
      { let loc = getLoc $1
        in
          L loc $ Params (reverse $ map unLoc $1) False
      }
  | parameter_list ',' '...'
      { let loc = combineLocs (getLoc $3 : map getLoc $1)
        in
          L loc $ Params (reverse $ map unLoc $1) True
      }

-- List built in reverse order
parameter_list :: { [L Param] }
parameter_list:
    parameter_declaration
      { [$1] }
  | ANTI_PARAMS
      { [L (getLoc $1) $ AntiParams (getANTI_PARAMS $1)] }
  | parameter_list ',' parameter_declaration
      { $3 : $1 }
  | parameter_list ',' ANTI_PARAMS
      { (L (getLoc $2) $ AntiParams (getANTI_PARAMS $3)) : $1 }

parameter_declaration :: { L Param }
parameter_declaration:
    declaration_specifiers
      {% do{ let (declSpec, decl) = unLoc $1
           ; let loc              = getLoc $1
           ; return $ L loc $ Param Nothing declSpec decl
           }
      }
  | declaration_specifiers declarator
      {% do{ let (declSpec, declRoot) = unLoc $1
           ; let (id, declToDecl)     = $2
           ; let L declLoc decl       = declToDecl (L (getLoc $1) declRoot)
           ; let loc                  = getLoc $1 <--> declLoc
           ; return $ L loc $ Param (Just $ unLoc id) declSpec decl
           }
      }
  | declaration_specifiers abstract_declarator
      {% do{ let (declSpec, declRoot) = unLoc $1
           ; let L declLoc decl       = $2 (L (getLoc $1) declRoot)
           ; let loc                  = getLoc $1 <--> declLoc
           ; return $ L loc $ Param Nothing declSpec decl
           }
      }
  | ANTI_PARAM
      { L (getLoc $1) (AntiParam (getANTI_PARAM $1)) }

{-
 - Added: The type_declaration rule is the parameter_declaration rule without
 - the ANTI_PARAM option. This allows us to parse type declarations easily for
 - later antiquoting.
 -}

type_declaration :: { L Type }
type_declaration:
    declaration_specifiers
      {% do{ let (declSpec, decl) = unLoc $1
           ; let loc              = getLoc $1
           ; return $ L loc $ Type declSpec decl
           }
      }
  | declaration_specifiers declarator
      {% do{ let (declSpec, declRoot) = unLoc $1
           ; let (id, declToDecl)     = $2
           ; let L declLoc decl       = declToDecl (L (getLoc $1) declRoot)
           ; let loc                  = getLoc $1 <--> declLoc
           ; return $ L loc $ Type declSpec decl
           }
      }
  | declaration_specifiers abstract_declarator
      {% do{ let (declSpec, declRoot) = unLoc $1
           ; let L declLoc decl       = $2 (L (getLoc $1) declRoot)
           ; let loc                  = getLoc $1 <--> declLoc
           ; return $ L loc $ Type declSpec decl
           }
      }

-- List built in reverse order
identifier_list :: { [L Id] }
identifier_list :
    identifier
      { [$1] }
  | identifier_list ',' identifier
      { $3 : $1 }

type_name :: { L Type }
type_name :
    specifier_qualifier_list
      {% do{ let loc = combineLocs $ map getLoc $1
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (map unLoc $1)
           ; return $ L loc $ Type declSpec DeclRoot
           }
      }
  | specifier_qualifier_list abstract_declarator
      {% do{ let specs_loc  = combineLocs $ map getLoc $1
           ; let L loc decl = $2 (L specs_loc DeclRoot)
           ; let loc'       = combineLocs (loc : map getLoc $1)
           ; declSpec <- withLocContext loc empty $
                         mkDeclSpec (map unLoc $1)
           ; return $ L loc $ Type declSpec decl
           }
      }
  | ANTI_TYPE
      { L (getLoc $1) $ AntiType (getANTI_TYPE $1) }
  | ANTI_TYPE abstract_declarator
      { let { v = getANTI_TYPE $1
            ; L loc decl = $2 (L (getLoc $1) (AntiTypeDecl v))
            }
        in
          L loc $
          Type (AntiTypeDeclSpec [] [] v) decl
      }

abstract_declarator :: { L Decl -> L Decl }
abstract_declarator :
    pointer
      { $1 }
  | direct_abstract_declarator
      { $1 }
  | pointer direct_abstract_declarator
      { $2 . $1 }

direct_abstract_declarator :: { L Decl -> L Decl }
direct_abstract_declarator :
    '(' abstract_declarator ')'
      { let { declToDecl = $2
            ; loc        = getLoc $1 <--> getLoc $3
            }
        in
          (\decl -> let L _ decl' = declToDecl decl
                    in
                      L loc decl')
      }
  | '(' abstract_declarator error
      {% do{ let decl = $2 (L (getLoc $1) DeclRoot)
           ; let loc  = getLoc $1 <--> getLoc decl
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | '[' ']'
      { \(L loc decl) ->
            L (getLoc $1 <--> getLoc $2)
                  (mkArray Nothing decl)
      }
  | '[' error
      {% throwExceptionAt (getLoc $1) (Unclosed "[") }
  | '[' constant_expression ']'
      { \(L loc decl) ->
            L (getLoc $1 <--> getLoc $3)
                  (mkArray (Just $ unLoc $2) decl)
      }
  | '[' constant_expression error
      {% do{ let loc = getLoc $1 <--> getLoc $2
           ; throwExceptionAt loc (Unclosed "[")
           }
      }
  | direct_abstract_declarator '[' ']'
      { $1 . (\(L loc decl) -> L  (loc <--> getLoc $3)
                                  (mkArray Nothing decl))
      }
  | direct_abstract_declarator '[' error
      {% throwExceptionAt (getLoc $2) (Unclosed "[") }
  | direct_abstract_declarator '[' constant_expression ']'
      { $1 . (\(L loc decl) ->
                  L (loc <--> getLoc $4)
                        (mkArray (Just $ unLoc $3) decl))
      }
  | direct_abstract_declarator '[' constant_expression error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "[")
           }
      }
  | direct_abstract_declarator '(' parameter_type_list ')'
      { $1 . (\(L loc decl) ->
                  L (loc <--> getLoc $4)
                        (mkProto (unLoc $3) decl))
      }
  | direct_abstract_declarator '(' parameter_type_list error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | direct_abstract_declarator '(' identifier_list ')'
      { $1 . (\(L loc decl) ->
                  L (loc <--> getLoc $4)
                        (mkOldProto (reverse $ map unLoc $3) decl))
      }
  | direct_abstract_declarator '(' identifier_list error
      {% do{ let loc = getLoc $2 <--> getLoc (last $3)
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | direct_abstract_declarator '(' ')'
      { $1 . (\(L loc decl) ->
                  L (loc <--> getLoc $3)
                        (mkOldProto [] decl))
      }
  | direct_abstract_declarator '(' error
      {% throwExceptionAt (getLoc $2) (Unclosed "(") }

typedef_name :: { L TySpec }
typedef_name :
    NAMED
      { L (getLoc $1) $ TSnamed $ Id (getNAMED $1) }
  | 'typename' identifier
      { let loc = getLoc $1 <--> getLoc $2
        in
          L (getLoc $2) $ TSnamed $ (unLoc $2)
      }
  | 'typename' error
      {% throwExceptionAt ((locEnd . getLoc) $1) (Expected ["identifier"]) }

initializer :: { L Initializer }
initializer :
    assignment_expression
      { L (getLoc $1) $ ExpInitializer (unLoc $1) }
  | '{' initializer_list '}'
      { let loc = getLoc $1 <--> getLoc $3
        in
          L loc $ CompoundInitializer (reverse $ map unLoc $2)
      }
  | '{' initializer_list error
      {% do{ let loc = getLoc $1 <--> getLoc (last $2)
           ; throwExceptionAt loc (Unclosed "{")
           }
      }
  | '{' initializer_list ',' '}'
      { let loc = getLoc $1 <--> getLoc $4
        in
          L loc $ CompoundInitializer (reverse $ map unLoc $2)
      }
  | '{' initializer_list ',' error
      {% do{ let loc = getLoc $1 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "{")
           }
      }

-- List built in reverse order
initializer_list :: { [L (Maybe Designation, Initializer)] }
initializer_list :
    initializer
      { [L (getLoc $1) (Nothing, unLoc $1)] }
  | designation initializer
      {% do{ let  loc = getLoc $1 <--> getLoc $2
           ; c99 <- getC99
           ; if c99
             then return $ [L loc (Just $ unLoc $1, unLoc $2)]
             else throwExceptionAt (getLoc $1) C99Designation
           }
      }
  | initializer_list ',' initializer
      { L (getLoc $3) (Nothing, unLoc $3) : $1 }
  | initializer_list ',' designation initializer
      {% do{ let loc = getLoc $3 <--> getLoc $4
           ; c99 <- getC99
           ; if c99
             then return $ L loc (Just $ unLoc $3, unLoc $4) : $1
             else throwExceptionAt (getLoc $3) C99Designation
           }
      }

{- Added: designations are part of C99 -}

designation :: { L Designation }
designation :
    designator_list '='
      { let loc = getLoc $2 <--> getLoc (last $1)
        in
          L loc $ Designation $ reverse $ map unLoc $1
      }

-- List built in reverse order
designator_list :: { [L Designator] }
designator_list :
    designator                 { [$1] }
  | designator_list designator { $2 : $1 }

designator :: { L Designator }
designator :
    '[' constant_expression ']'
      { let loc = getLoc $1 <--> getLoc $3
        in
          L loc $ IndexDesignator $ unLoc $2
      }
  | '.' identifier
      { let loc = getLoc $1 <--> getLoc $2
        in
          L loc $ MemberDesignator $ unLoc $2
      }

{------------------------------------------------------------------------------
 -
 - Statements
 -
 ------------------------------------------------------------------------------}

statement :: { L Stm }
statement :
    labeled_statement    { $1 }
  | compound_statement
      { let (initgroups, stms) = unLoc $1
        in
          locate (getLoc $1) (Block (map unLoc initgroups) (map unLoc stms))
      }
  | expression_statement { $1 }
  | selection_statement  { $1 }
  | iteration_statement  { $1 }
  | jump_statement       { $1 }
  | asm_statement        { $1 }
  | ANTI_STM             { locate (getLoc $1) (AntiStm (getANTI_STM $1)) }
  | atomic_statement     { $1 }

labeled_statement :: { L Stm }
labeled_statement :
    identifier ':' statement
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ Label (unLoc $1) (unLoc $3)
      }
  | 'case' constant_expression ':' statement
      { let loc = getLoc $1 <--> getLoc $4
        in
          locate loc $ Case (unLoc $2) (unLoc $4)
      }
  | 'default' ':' statement
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ Default (unLoc $3)
      }

compound_statement :: { L ([L InitGroup], [L Stm]) }
compound_statement:
    '{' begin_scope end_scope '}'
      { let loc = getLoc $1 <--> getLoc $4
        in
          L loc ([], [])
      }
  | '{' begin_scope error
      {% throwExceptionAt (getLoc $3) (Unclosed "{") }
  | '{' begin_scope declaration_list end_scope '}'
      {% do { let loc = getLoc $1 <--> getLoc $5
            ; return $ L loc (reverse $3, [])
            }
      }
  | '{' begin_scope declaration_list error
      {% do { let loc = getLoc $1 <--> (last $3)
            ; throwExceptionAt loc (Unclosed "(")
            }
      }
  | '{' begin_scope statement_list end_scope '}'
      {% do { let loc = getLoc $1 <--> getLoc $5
            ; return $ L loc ([], reverse $3)
            }
      }
  | '{' begin_scope statement_list error
      {% do { let loc = getLoc $1 <--> getLoc (last $3)
            ; throwExceptionAt loc (Unclosed "{")
            }
      }
  | '{' begin_scope declaration_list statement_list end_scope '}'
      {% do { let loc = getLoc $1 <--> getLoc $6
            ; return $ L loc (reverse $3, reverse $4)
            }
      }
  | '{' begin_scope declaration_list statement_list error
      {% do { let loc = getLoc $1 <--> getLoc (last $4)
            ; throwExceptionAt loc (Unclosed "{")
            }
      }

begin_scope :: { () }
begin_scope : {% pushScope }

end_scope :: { () }
end_scope : {% popScope }

-- List built in reverse order
declaration_list :: { [L InitGroup] }
declaration_list :
    declaration
      { [$1] }
  | ANTI_DECLS
      { [L (getLoc $1) $ AntiDecls (getANTI_DECLS $1)] }
  | declaration_list declaration
      { $2 : $1 }
  | declaration_list ANTI_DECLS
      { (L (getLoc $2) $ AntiDecls (getANTI_DECLS $2)) : $1 }

-- List built in reverse order
statement_list :: { [L Stm] }
statement_list:
    statement
      { [$1] }
  | ANTI_STMS
      { [locate (getLoc $1) $ AntiStms (getANTI_STMS $1)] }
  | statement_list statement
      { $2 : $1 }
  | statement_list ANTI_STMS
      { (locate (getLoc $2) $ AntiStms (getANTI_STMS $2)) : $1 }

expression_statement :: { L Stm }
expression_statement:
    ';'
      { locate (getLoc $1) $ Exp Nothing }
  | expression ';'
      { let (L loc exp) = $1
        in
          locate (getLoc $1 <--> getLoc $2) (Exp (Just exp))
      }
  | expression error
      {% throwExceptionAt ((locEnd . getLoc) $1) (Expected ["';'"]) }

selection_statement :: { L Stm }
selection_statement :
    'if' '(' expression ')' statement
      { let loc = getLoc $1 <--> getLoc $5
        in
          locate loc $ If (unLoc $3) (unLoc $5) Nothing
      }
  | 'if' '(' expression ')' statement 'else' statement
      { let loc = getLoc $1 <--> getLoc $7
        in
          locate loc $ If (unLoc $3) (unLoc $5) (Just $ unLoc $7)
      }
  | 'if' '(' expression error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | 'switch' '(' expression ')' statement
      { let loc = getLoc $1 <--> getLoc $5
        in
          locate loc $ Switch (unLoc $3) (unLoc $5)
      }
  | 'switch' '(' expression error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "(")
           }
      }

iteration_statement :: { L Stm }
iteration_statement :
    'while' '(' expression ')' statement
      { let loc = getLoc $1 <--> getLoc $5
        in
          locate loc $ While (unLoc $3) (unLoc $5)
      }
  | 'while' '(' expression error
      {% do{ let loc = getLoc $2 <--> getLoc $3
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | 'do' statement 'while' '(' expression ')' ';'
      { let loc = getLoc $1 <--> getLoc $7
        in
          locate loc $ DoWhile (unLoc $2) (unLoc $5)
      }
  | 'do' statement 'while' '(' expression error
      {% do{ let loc = getLoc $4 <--> getLoc $5
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | 'for' '(' for_expression_statement for_expression_statement ')' statement
      { let loc = getLoc $1 <--> getLoc $6
        in
          locate loc $ For (unLoc $3) (unLoc $4) Nothing (unLoc $6)
      }
  | 'for' '(' for_expression_statement for_expression_statement error
      {% do{ let loc = getLoc $2 <--> getLoc $4
           ; throwExceptionAt loc (Unclosed "(")
           }
      }
  | 'for' '(' for_expression_statement for_expression_statement expression ')' statement
      { let loc = getLoc $1 <--> getLoc $7
        in
          locate loc $ For (unLoc $3) (unLoc $4) (Just $ unLoc $5) (unLoc $7)
      }
  | 'for' '(' for_expression_statement for_expression_statement expression error
      {% do{ let loc = getLoc $2 <--> getLoc $5
           ; throwExceptionAt loc (Unclosed "(")
           }
      }

{- This makes it easier to parse for statements -}
for_expression_statement :: { L (Maybe Exp)  }
for_expression_statement:
    ';'
      { L (getLoc $1) Nothing }
  | expression ';'
      { let (L loc exp) = $1
        in
          L (getLoc $1 <--> getLoc $2) (Just exp)
      }

jump_statement :: { L Stm }
jump_statement :
    'goto' identifier ';'
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ Goto (unLoc $2)
      }
  | 'continue' ';'
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ Continue
      }
  | 'break' ';'
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ Break
      }
  | 'return' ';'
      { let loc = getLoc $1 <--> getLoc $2
        in
          locate loc $ Return Nothing
      }
  | 'return' expression ';'
      { let loc = getLoc $1 <--> getLoc $3
        in
          locate loc $ Return (Just $ unLoc $2)
      }

{------------------------------------------------------------------------------
 -
 - External definitions
 -
 ------------------------------------------------------------------------------}

translation_unit :: { [L Definition] }
translation_unit :
    translation_unit_ { reverse $1 }

-- List built in reverse order
translation_unit_ :: { [L Definition] }
translation_unit_ :
    external_declaration
      { [$1] }
  | ANTI_EDECLS
      { [locate (getLoc $1) $ AntiEdecls (getANTI_EDECLS $1)] }
  | translation_unit_ external_declaration
      { $2 : $1 }
  | translation_unit_ ANTI_EDECLS
      { (locate (getLoc $2) $ AntiEdecls (getANTI_EDECLS $2)) : $1 }

external_declaration :: { L Definition }
external_declaration :
    function_definition
      { locate (getLoc $1) $ FuncDef (unLoc $1) }
  | declaration
      { locate (getLoc $1) $ DecDef (unLoc $1) }
  | ANTI_FUNC
      { locate (getLoc $1) $ AntiFunc (getANTI_FUNC $1) }
  | ANTI_EDECL
      { locate (getLoc $1) $ AntiEdecl (getANTI_EDECL $1) }

function_definition :: { L Func }
function_definition :
    declaration_specifiers declarator compound_statement
      {% do{ let (declSpec, declRoot) =  unLoc $1
           ; let (id, declToDecl)     =  $2
           ; let (initGroups, stms)   =  unLoc $3
           ; let L declLoc decl       =  declToDecl (L (getLoc $1) declRoot)
           ; let loc                  =  combineLocs (getLoc $1 : declLoc :
                                                      map getLoc initGroups ++
                                                      map getLoc stms)
           ; let stmt                 =  Block  (map unLoc initGroups)
                                                (map unLoc stms)
                                                (SrcLoc loc)
           ; case decl of
               { Proto protoDecl args ->
                     return $ L loc $
                            Func declSpec (unLoc id)
                                 protoDecl args stmt
               ; OldProto protoDecl args ->
                   return $ L loc $
                          OldFunc declSpec (unLoc id)
                                  protoDecl args Nothing stmt
               ; _ -> throwExceptionAt loc BadFunctionDeclaration
               }
           }
      }
  | declaration_specifiers declarator declaration_list compound_statement
      {% do{ let (declSpec, declRoot) =  unLoc $1
           ; let (id, declToDecl)     =  $2
           ; let argDecls             =  $3
           ; let (initGroups, stms)   =  unLoc $4
           ; let L declLoc decl       =  declToDecl (L (getLoc $1) declRoot)
           ; let loc                  =  combineLocs (getLoc $1 : declLoc :
                                                      map getLoc initGroups ++
                                                      map getLoc stms)
           ; let stmt                 =  Block  (map unLoc initGroups)
                                                (map unLoc stms)
                                                (SrcLoc loc)
           ; case decl of
               { OldProto protoDecl args ->
                     return $ L loc $
                            OldFunc declSpec (unLoc id)
                                    protoDecl args
                                    (Just $ reverse $ map unLoc argDecls) stmt
               ; _ -> throwExceptionAt loc BadFunctionDeclaration
               }
           }
      }

{------------------------------------------------------------------------------
 -
 - GCC extensions
 -
 ------------------------------------------------------------------------------}

attributes :: { [L Attr] }
attributes :
    attribute            { $1 }
  | attributes attribute { $1 ++ $2 }

attribute :: { [L Attr] }
attribute :
    'attribute' '(' '(' attribute_list ')' ')' { $4 }

-- List built in reverse order
attribute_list :: { [L Attr] }
attribute_list :
    attrib                    { [$1] }
  | attribute_list ',' attrib { $3 : $1 }

attrib :: { L Attr }
attrib :
    attrib_name
      { L (getLoc $1) $ Attr (unLoc $1) []}
  | attrib_name '(' argument_expression_list ')'
      { let loc = getLoc $1 <--> getLoc $4
        in
          L loc $ Attr (unLoc $1) (reverse $ map unLoc $3)
      }

attrib_name :: { L Id }
attrib_name :
    identifier_or_typename { $1 }
  | 'static'               { L (getLoc $1) $ Id "static" }
  | 'extern'               { L (getLoc $1) $ Id "extern" }
  | 'register'             { L (getLoc $1) $ Id "register" }
  | 'typedef'              { L (getLoc $1) $ Id "typedef" }
  | 'inline'               { L (getLoc $1) $ Id "inline" }
  | 'auto'                 { L (getLoc $1) $ Id "auto" }
  | 'const'                { L (getLoc $1) $ Id "const" }
  | 'volatile'             { L (getLoc $1) $ Id "volatile" }
  | 'unsigned'             { L (getLoc $1) $ Id "unsigned" }
  | 'long'                 { L (getLoc $1) $ Id "long" }
  | 'short'                { L (getLoc $1) $ Id "short" }
  | 'signed'               { L (getLoc $1) $ Id "signed" }
  | 'int'                  { L (getLoc $1) $ Id "int" }
  | 'char'                 { L (getLoc $1) $ Id "char" }
  | 'float'                { L (getLoc $1) $ Id "float" }
  | 'double'               { L (getLoc $1) $ Id "double" }
  | 'void'                 { L (getLoc $1) $ Id "void" }

asm_statement :: { L Stm }
asm_statement :
    'asm' '(' asm_template ')'
      { let loc = getLoc $1 <--> getLoc $4
        in
          locate loc $ Asm [] (reverse $3) [] [] []
      }
  | 'asm' '(' asm_template ':' asm_inouts ')'
      { let loc = getLoc $1 <--> getLoc $6
        in
          locate loc $ Asm [] (reverse $3) $5 [] []
      }
  | 'asm' '(' asm_template ':' asm_inouts ':' asm_inouts ')'
      { let loc = getLoc $1 <--> getLoc $8
        in
          locate loc $ Asm [] (reverse $3) $5 $7 []
      }
  | 'asm' '(' asm_template ':' asm_inouts ':' asm_inouts ':' asm_clobbers ')'
      { let loc = getLoc $1 <--> getLoc $10
        in
          locate loc $ Asm [] (reverse $3) $5 $7 $9
      }

atomic_statement :: { L Stm }
atomic_statement :
  'atomic' statement
    { let loc = $1 <--> $2
      in
        locate loc $ Atomic (unLoc $2)
    }

-- List built in reverse order
asm_template :: { [String] }
asm_template :
    STRING              { [getSTRING $1] }
  | asm_template STRING { getSTRING $2 : $1 }

asm_inouts :: { [(String, Exp)] }
asm_inouts :
    {- empty -}   { [] }
  | asm_inouts_ne { reverse $1 }

-- List built in reverse order
asm_inouts_ne :: { [(String, Exp)] }
asm_inouts_ne:
    asm_inout                   { [$1] }
  | asm_inouts_ne ',' asm_inout { $3 : $1 }

asm_inout :: { (String, Exp) }
asm_inout :
    STRING '(' expression ')' { (getSTRING $1, unLoc $3) }

asm_clobbers :: { [String] }
asm_clobbers :
    {- empty -}     { [] }
  | asm_clobbers_ne { reverse $1 }

-- List built in reverse order
asm_clobbers_ne :: { [String] }
asm_clobbers_ne :
    asm_clobber                     { [$1] }
  | asm_clobbers_ne ',' asm_clobber { $3 : $1 }

asm_clobber :: { String }
asm_clobber :
    STRING { getSTRING $1 }

{------------------------------------------------------------------------------
 -
 - NesC extensions
 -
 ------------------------------------------------------------------------------}

nesc_file :: { L NesCFile }
nesc_file :
    includes_list interface_definition_or_component
     {% let loc = combineLocs (map getLoc $1)
        in
         throwExceptionAt loc IncludesDeprecated
      }
  | opt_translation_unit interface_definition_or_component
      { let loc = combineLocs (getLoc $2 : map getLoc $1)
        in
          L loc $ NesCFile (map unLoc $1) (unLoc $2)
      }

opt_translation_unit :: { [L Definition] }
opt_translation_unit :
    {- empty -}      { [] }
  | translation_unit { $1 }

interface_definition_or_component :: { L (Either InterfaceDef ComponentDef) }
interface_definition_or_component :
    interface_definition { L (getLoc $1) $ Left  $ unLoc $1 }
  | component            { L (getLoc $1) $ Right $ unLoc $1 }

opt_includes_list :: { [L Includes] }
opt_includes_list :
    {- empty -}   { [] }
  | includes_list { reverse $1 }

-- List built in reverse order
includes_list :: { [L Includes] }
includes_list :
    includes               { [$1] }
  | includes_list includes { $2 : $1 }

includes :: { L Includes }
includes :
  'includes' identifier_list ';'
    { let loc = combineLocs [getLoc $1, getLoc $3]
      in
        L loc $ Includes $ reverse $ map unLoc $2
    }

interface_definition :: { L InterfaceDef }
interface_definition :
    'interface' identifier '{' '}'
      {% let { L _ (Id id) = $2
             ; loc = combineLocs [getLoc $1, getLoc $4]
             }
         in
           throwExceptionAt loc (EmptyInterface id)
      }
  | 'interface' identifier type_parameters '{' '}'
      {% let { L _ (Id id) = $2
             ; loc = combineLocs [getLoc $1, getLoc $5]
             }
         in
           throwExceptionAt loc (EmptyInterface id)
      }
  | 'interface' identifier '{' begin_scope declaration_list end_scope '}'
      { let loc = combineLocs [getLoc $1, getLoc $7]
        in
          L loc $ InterfaceDef (unLoc $2) [] (reverse $ map unLoc $5)
      }
  | 'interface' identifier type_parameters '{' begin_scope declaration_list end_scope '}'
      { let loc = combineLocs [getLoc $1, getLoc $8]
        in
          L loc $ InterfaceDef (unLoc $2) (reverse $ map unLoc $3)
                                          (reverse $ map unLoc $6)
      }

type_parameters :: { [L TypeParam] }
type_parameters :
    '<' type_parameter_list '>' { reverse $2 }

-- List built in reverse order
type_parameter_list :: { [L TypeParam] }
type_parameter_list :
    identifier
      { [L (getLoc $1) $ TypeParam (unLoc $1) []] }
  | identifier attributes
      { let loc = combineLocs (getLoc $1 : map getLoc $2)
        in
          [L loc $ TypeParam (unLoc $1) (map unLoc $2)]
      }
  | type_parameter_list ',' identifier
      { (L (getLoc $3) $ TypeParam (unLoc $3) []) : $1 }
  | type_parameter_list ',' identifier attributes
      { let loc = combineLocs (getLoc $3 : map getLoc $4)
        in
          (L loc $ TypeParam (unLoc $3) (map unLoc $4)) : $1
      }

component :: { L ComponentDef }
component :
    'module' identifier comp_spec module_imp
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          L loc $ Module False (unLoc $2) []
                               (map unLoc $3) (unLoc $4)
      }
  | 'generic' 'module' identifier comp_params comp_spec module_imp
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          L loc $ Module True (unLoc $3) (map unLoc $4)
                              (map unLoc $5) (unLoc $6)
      }
  | 'configuration' identifier comp_spec config_imp
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          L loc $ Config False (unLoc $2) []
                               (map unLoc $3) (unLoc $4)
      }
  | 'generic' 'configuration' identifier comp_params comp_spec config_imp
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          L loc $ Config True (unLoc $3) (map unLoc $4)
                              (map unLoc $5) (unLoc $6)
      }
  | 'component' identifier comp_spec
      { let loc = combineLocs (getLoc $1 : map getLoc $3)
        in
          L loc $ Component (unLoc $2) (map unLoc $3)
      }

comp_params :: { [L CompParam] }
comp_params :
  '(' comp_param_list ')' { reverse $2 }

-- List built in reverse order
comp_param_list :: { [L CompParam] }
comp_param_list:
    comp_param                     { [$1] }
  | comp_param_list ',' comp_param { $3 : $1 }

comp_param :: { L CompParam }
comp_param :
    parameter_declaration
      { L (getLoc $1) $ CompArgParam (unLoc $1) }
  | 'typedef' identifier
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          L loc $ CompTypeParam (unLoc $2)
      }

comp_spec :: { [L UsesProvides] }
comp_spec :
    '{' '}'                    { [] }
  | '{' uses_provides_list '}' { reverse $2 }

-- List built in reverse order
uses_provides_list :: { [L UsesProvides] }
uses_provides_list :
    uses_provides
      { [$1] }
  | ANTI_USES_PROVIDES_LIST
      { [L (getLoc $1) $ AntiUsesProvidesList (getANTI_USES_PROVIDES_LIST $1)] }
  | uses_provides_list uses_provides
      { $2 : $1 }
  | uses_provides_list ANTI_USES_PROVIDES_LIST
      { L (getLoc $2) (AntiUsesProvidesList (getANTI_USES_PROVIDES_LIST $2)) : $1 }

uses_provides :: { L UsesProvides }
uses_provides :
    'uses' spec_elem_list
      { let loc = combineLocs (getLoc $1 : map getLoc $2)
        in
          L loc $ Uses (map unLoc $2)
      }
  | 'provides' spec_elem_list
      { let loc = combineLocs (getLoc $1 : map getLoc $2)
        in
          L loc $ Provides (map unLoc $2)
      }
  | ANTI_USES_PROVIDES
      { L (getLoc $1) (AntiUsesProvides (getANTI_USES_PROVIDES $1)) }

spec_elem_list :: { [L SpecElem] }
spec_elem_list :
    spec_element          { [$1] }
  | '{' spec_elements '}' { reverse $2 }

-- List built in reverse order
spec_elements :: { [L SpecElem] }
spec_elements:
    spec_element               { [$1] }
  | spec_elements spec_element { $2 : $1 }

spec_element :: { L SpecElem }
spec_element :
    declaration
      { let loc = getLoc $1
        in
          L loc $ BareSpec (unLoc $1)
      }
  | 'interface' identifier type_arguments ';'
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          L loc $ InterfaceSpec (unLoc $2) (map unLoc $3)
                                Nothing []
      }
  | 'interface' identifier type_arguments 'as' identifier ';'
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          L loc $ InterfaceSpec (unLoc $2) (map unLoc $3)
                                (Just $ unLoc $5) []
      }
  | 'interface' identifier type_arguments instance_parameters ';'
      { let loc = combineLocs [getLoc $1, getLoc $5]
        in
          L loc $ InterfaceSpec (unLoc $2) (map unLoc $3)
                                Nothing (map unLoc $4)
      }
  | 'interface' identifier type_arguments 'as' identifier instance_parameters ';'
      { let loc = combineLocs [getLoc $1, getLoc $7]
        in
          L loc $ InterfaceSpec (unLoc $2) (map unLoc $3)
                                (Just $ unLoc $5) (map unLoc $6)
      }

type_arguments :: { [L Type] }
type_arguments :
    {- empty -}           { [] }
  | '<' type_arg_list '>' { reverse $2 }

-- List built in reverse order
type_arg_list :: { [L Type] }
type_arg_list :
    type_name                   { [$1] }
  | type_arg_list ',' type_name { $3 : $1 }

module_imp :: { L ModuleImp }
module_imp :
    error
      {% throwExceptionAt ((locEnd . getLoc) $1) (Expected ["'implementation'"]) }
  | 'implementation' '{' begin_scope translation_unit end_scope '}'
      { let loc = combineLocs [getLoc $1, getLoc $6]
        in
          L loc $ ModuleImp (map unLoc $4)
      }
  | 'implementation' '{' begin_scope translation_unit error
      {% let loc = locEnd $ combineLocs $ map getLoc $4
         in
           throwExceptionAt loc (Unclosed "{")
      }

config_imp :: { L ConfigImp }
config_imp :
    error
      {% throwExceptionAt ((locEnd . getLoc) $1) (Expected ["'implementation'"]) }
  | 'implementation' '{' error
      {% throwExceptionAt ((locEnd . getLoc) $1) (Expected ["component list",
                                                            "connection list",
                                                            "'}'"]) }
  | 'implementation' '{' '}'
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          L loc $ ConfigImp [] []
      }
  | 'implementation' '{' component_list '}'
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          L loc $ ConfigImp (reverse $ map unLoc $3) []
      }
  | 'implementation' '{' component_list error
      {% let loc = locEnd $ combineLocs $ map getLoc $3
         in
           throwExceptionAt loc (Expected ["connection list", "'}'"])
      }
  | 'implementation' '{' connection_list '}'
      { let loc = combineLocs [getLoc $1, getLoc $4]
        in
          L loc $ ConfigImp [] (reverse $ map unLoc $3)
      }
  | 'implementation' '{' component_list connection_list '}'
      { let loc = combineLocs [getLoc $1, getLoc $5]
        in
          L loc $ ConfigImp (reverse $ map unLoc $3) (reverse $ map unLoc $4)
      }

-- List built in reverse order
component_list :: { [L Components] }
component_list :
    components
      { [$1] }
  | ANTI_COMPONENTS_LIST
      { [L (getLoc $1) $ AntiComponentsList (getANTI_COMPONENTS_LIST $1)] }
  | component_list components
      { $2 : $1 }
  | component_list ANTI_COMPONENTS_LIST
      { L (getLoc $2) (AntiComponentsList (getANTI_COMPONENTS_LIST $2)) : $1 }

components :: { L Components }
components :
    'components' component_line ';'
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          L loc $ Components (reverse $ map unLoc $2)
      }
  | ANTI_COMPONENTS
      { L (getLoc $1) (AntiComponents (getANTI_COMPONENTS $1)) }

-- List built in reverse order
component_line :: { [L (Component, Maybe Id)] }
component_line:
    component_ref                    { [$1] }
  | component_line ',' component_ref { $3 : $1 }

component_ref :: { L (Component, Maybe Id) }
component_ref :
    identifier
      { let loc = getLoc $1
        in
          L loc $ (StandardComponent (unLoc $1), Nothing)
      }
  | identifier 'as' identifier
      { let loc = combineLocs [getLoc $1, getLoc $2]
        in
          L loc $ (StandardComponent (unLoc $1), Just (unLoc $3))
      }
  | 'new' identifier '(' comp_arg_list ')'
      { let loc = combineLocs [getLoc $1, getLoc $5]
        in
          L loc $ (GenericComponent (unLoc $2) (map unLoc $4), Nothing)
      }
  | 'new' identifier '(' comp_arg_list ')' 'as' identifier
      { let loc = combineLocs [getLoc $1, getLoc $7]
        in
          L loc $ (GenericComponent (unLoc $2) (map unLoc $4), Just (unLoc $7))
      }

-- List built in reverse order
comp_arg_list :: { [L CompArg] }
comp_arg_list :
    comp_arg                   { [$1] }
  | comp_arg_list ',' comp_arg { $3 : $1 }

comp_arg :: { L CompArg }
comp_arg :
    assignment_expression
      { let L loc exp = $1
        in
          L loc $ CompExpArg exp
      }
  | type_name
      { let L loc ty = $1
        in
          L loc $ CompTypeArg ty
      }

-- List built in reverse order
connection_list :: { [L Connection] }
connection_list :
    connection ';'
      { [$1] }
  | ANTI_CONNECTIONS
      { [L (getLoc $1) $ AntiConnections (getANTI_CONNECTIONS $1)] }
  | connection_list connection
      { $2 : $1 }
  | connection_list ANTI_CONNECTIONS
      { L (getLoc $2) (AntiConnections (getANTI_CONNECTIONS $2)) : $1 }

connection :: { L Connection }
connection :
    endpoint error
      {% throwExceptionAt ((locEnd . getLoc) $1) (Expected ["'='", "'->'", "'<-'", "';'"]) }
  | endpoint '=' endpoint
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          L loc $ ConnEqual (unLoc $1) (unLoc $3)
      }
  | endpoint '->' endpoint
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          L loc $ ConnFromTo (unLoc $1) (unLoc $3)
      }
  | endpoint '<-' endpoint
      { let loc = combineLocs [getLoc $1, getLoc $3]
        in
          L loc $ ConnToFrom (unLoc $1) (unLoc $3)
      }
  | ANTI_CONNECTION
      { L (getLoc $1) (AntiConnection (getANTI_CONNECTION $1)) }

endpoint :: { L Endpoint }
endpoint :
    identifier_path
      { let loc = combineLocs $ map getLoc $1
        in
          L loc $ Endpoint (reverse $ map unLoc $1) []
      }
  | identifier_path '[' argument_expression_list ']'
      { let loc = combineLocs $ getLoc $4 : map getLoc $1
        in
          L loc $ Endpoint (reverse $ map unLoc $1) (reverse $ map unLoc $3)
      }

-- List built in reverse order
identifier_path :: { [L Id] }
identifier_path :
    identifier                     { [$1] }
  | identifier_path '.' identifier { $3 : $1 }

instance_parameters :: { [L Param] }
instance_parameters :
    '[' error
      {% throwExceptionAt (getLoc $1) (Unclosed "[") }
  | '[' parameter_list error
      {% do{ let loc = combineLocs (getLoc $1 : map getLoc $2)
           ; throwExceptionAt loc (Unclosed "[")
           }
      }
  | '[' parameter_list ']'
      { reverse $2 }

{
happyError :: L T.Token -> P a
happyError (L (Loc start _) _) = throwExceptionAt start ParserError

getCHAR        (L _ (T.TcharConst x))        = x
getSTRING      (L _ (T.TstringConst x))      = x
getINT         (L _ (T.TintConst x))         = x
getLONG        (L _ (T.TlongIntConst x))     = x
getLONG_LONG   (L _ (T.TlongLongIntConst x)) = x
getFLOAT       (L _ (T.TfloatConst x))       = x
getDOUBLE      (L _ (T.TdoubleConst x))      = x
getLONG_DOUBLE (L _ (T.TlongDoubleConst x))  = x
getID          (L _ (T.Tidentifier id))      = id
getNAMED       (L _ (T.Tnamed id))           = id

getANTI_ID          (L _ (T.Tanti_id v))          = v
getANTI_INT         (L _ (T.Tanti_int v))         = v
getANTI_UINT        (L _ (T.Tanti_uint v))        = v
getANTI_LINT        (L _ (T.Tanti_lint v))        = v
getANTI_ULINT       (L _ (T.Tanti_ulint v))       = v
getANTI_FLOAT       (L _ (T.Tanti_float v))       = v
getANTI_DOUBLE      (L _ (T.Tanti_double v))      = v
getANTI_LONG_DOUBLE (L _ (T.Tanti_long_double v)) = v
getANTI_CHAR        (L _ (T.Tanti_char v))        = v
getANTI_STRING      (L _ (T.Tanti_string v))      = v
getANTI_EXP         (L _ (T.Tanti_exp v))         = v
getANTI_FUNC        (L _ (T.Tanti_func v))        = v
getANTI_ARGS        (L _ (T.Tanti_args v))        = v
getANTI_DECL        (L _ (T.Tanti_decl v))        = v
getANTI_DECLS       (L _ (T.Tanti_decls v))       = v
getANTI_SDECL       (L _ (T.Tanti_sdecl v))       = v
getANTI_SDECLS      (L _ (T.Tanti_sdecls v))      = v
getANTI_ENUM        (L _ (T.Tanti_enum v))        = v
getANTI_ENUMS       (L _ (T.Tanti_enums v))       = v
getANTI_EDECL       (L _ (T.Tanti_edecl v))       = v
getANTI_EDECLS      (L _ (T.Tanti_edecls v))      = v
getANTI_STM         (L _ (T.Tanti_stm v))         = v
getANTI_STMS        (L _ (T.Tanti_stms v))        = v
getANTI_TYPE        (L _ (T.Tanti_type v))        = v
getANTI_PARAM       (L _ (T.Tanti_param v))       = v
getANTI_PARAMS      (L _ (T.Tanti_params v))      = v

getANTI_USES_PROVIDES      (L _ (T.Tanti_uses_provides v))      = v
getANTI_USES_PROVIDES_LIST (L _ (T.Tanti_uses_provides_list v)) = v
getANTI_COMPONENTS         (L _ (T.Tanti_components v))         = v
getANTI_COMPONENTS_LIST    (L _ (T.Tanti_components_list v))    = v
getANTI_CONNECTION         (L _ (T.Tanti_connection v))         = v
getANTI_CONNECTIONS        (L _ (T.Tanti_connections v))        = v

lexer :: (L T.Token -> P a) -> P a
lexer cont = do
    tok <- lexToken
    cont tok

locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))

data DeclTySpec = DeclTySpec DeclSpec
                | AntiDeclTySpec String

data TySpec = TSauto
            | TSregister
            | TSstatic
            | TSextern
            | TStypedef
            | TSconst
            | TSvolatile
            | TSinline
            | TSsigned
            | TSunsigned
            | TSvoid
            | TSchar
            | TSshort
            | TSint
            | TSlong
            | TSfloat
            | TSdouble
            | TSstruct (Maybe Id) (Maybe [FieldGroup]) [Attr]
            | TSunion (Maybe Id) (Maybe [FieldGroup]) [Attr]
            | TSenum (Maybe Id) [CEnum] [Attr]
            | TSnamed Id
            | TSva_list
            | TSasync
            | TScommand
            | TSdefault
            | TSevent
            | TSnorace
            | TStask

instance Pretty TySpec where
    ppr TSauto     = text "auto"
    ppr TSregister = text "register"
    ppr TSstatic   = text "static"
    ppr TSextern   = text "extern"
    ppr TStypedef  = text "typedef"
    ppr TSconst    = text "const"
    ppr TSvolatile = text "volatile"
    ppr TSinline   = text "inline"
    ppr TSsigned   = text "signed"
    ppr TSunsigned = text "unsigned"
    ppr TSvoid     = text "void"
    ppr TSchar     = text "char"
    ppr TSshort    = text "short"
    ppr TSint      = text "int"
    ppr TSlong     = text "long"
    ppr TSfloat    = text "float"
    ppr TSdouble   = text "double"

    ppr (TSstruct maybe_id maybe_fields attrs)
        = pprStructOrUnion "struct" maybe_id maybe_fields attrs

    ppr (TSunion maybe_id maybe_fields attrs)
        = pprStructOrUnion "union" maybe_id maybe_fields attrs

    ppr (TSenum maybe_id cenums attrs)
        = pprEnum maybe_id cenums attrs

    ppr (TSnamed id) = ppr id
    ppr TSva_list    = text "__builtin_va_list"
    ppr TSasync      = text "async"
    ppr TScommand    = text "command"
    ppr TSdefault    = text "default"
    ppr TSevent      = text "event"
    ppr TSnorace     = text "norace"
    ppr TStask       = text "task"

isStorage :: TySpec -> Bool
isStorage TSauto     = True
isStorage TSregister = True
isStorage TSstatic   = True
isStorage TSextern   = True
isStorage TStypedef  = True
isStorage TSasync    = True
isStorage TScommand  = True
isStorage TSdefault  = True
isStorage TSevent    = True
isStorage TSnorace   = True
isStorage TStask     = True
isStorage _          = False

mkStorage :: [TySpec] -> [Storage]
mkStorage specs = map mk (filter isStorage specs)
    where
      mk :: TySpec -> Storage
      mk TSauto     = Tauto
      mk TSregister = Tregister
      mk TSstatic   = Tstatic
      mk TSextern   = Textern
      mk TStypedef  = Ttypedef
      mk TSasync    = Tasync
      mk TScommand  = Tcommand
      mk TSdefault  = Tdefault
      mk TSevent    = Tevent
      mk TSnorace   = Tnorace
      mk TStask     = Ttask
      mk _          = error "internal error in mkStorage"

isTypeQual :: TySpec -> Bool
isTypeQual TSconst    = True
isTypeQual TSvolatile = True
isTypeQual TSinline   = True
isTypeQual _          = False

mkTypeQuals :: [TySpec] -> [TypeQual]
mkTypeQuals specs = map mk (filter isTypeQual specs)
    where
      mk :: TySpec -> TypeQual
      mk TSconst    = Tconst
      mk TSvolatile = Tvolatile
      mk TSinline   = Tinline
      mk _          = error "internal error in mkTypeQual"

isSign :: TySpec -> Bool
isSign TSsigned   = True
isSign TSunsigned = True
isSign _          = False

hasSign :: [TySpec] -> Bool
hasSign specs = any isSign specs

mkSign :: [TySpec] -> P (Maybe Sign)
mkSign specs =
    case filter isSign specs of
      []           -> return $ Nothing
      [TSunsigned] -> return $ Just Tunsigned
      [TSsigned]   -> return $ Just Tsigned
      [_]          -> return $ error "internal error in mkSign"
      _            -> fail "multiple signs specified"

checkNoSign :: [TySpec] -> String -> P ()
checkNoSign spec msg = if hasSign spec then fail msg else return ()

composeDecls :: Decl -> Decl -> Decl
composeDecls DeclRoot             root = root
composeDecls (C.Ptr quals decl)   root = C.Ptr quals (composeDecls decl root)
composeDecls (Array decl exp)     root = Array (composeDecls decl root) exp
composeDecls (Proto decl args)    root = Proto (composeDecls decl root) args
composeDecls (OldProto decl args) root = OldProto (composeDecls decl root) args

mkDeclSpec :: [TySpec] -> P DeclSpec
mkDeclSpec specs =
    go rest
  where
    storage ::[Storage]
    storage = mkStorage specs

    quals :: [TypeQual]
    quals = mkTypeQuals specs

    rest :: [TySpec]
    rest = filter (\x -> not (isStorage x)
                         && not (isTypeQual x)
                         && not (isSign x))
                  specs

    go :: [TySpec] -> P DeclSpec
    go [TSvoid] = do
        checkNoSign specs "sign specified for void type"
        return $ DeclSpec storage quals Tvoid

    go [TSchar] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tchar sign)

    go [TSshort] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tshort sign)

    go [TSshort, TSint ] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tshort sign)

    go [TSint, TSshort ] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tshort sign)

    go [TSint] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tint sign)

    go [TSlong] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tlong sign)

    go [TSlong, TSint ] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tlong sign)

    go [TSint, TSlong ] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tlong sign)

    go [TSlong, TSlong] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tlong_long sign)

    go [TSlong, TSlong, TSint ] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tlong_long sign)

    go [TSlong, TSint, TSlong ] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tlong_long sign)

    go [TSint, TSlong, TSlong] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tlong_long sign)

    go [TSfloat] = do
        checkNoSign specs "sign specified for float type"
        return $ DeclSpec storage quals Tfloat

    go [TSdouble] = do
        checkNoSign specs "sign specified for double type"
        return $ DeclSpec storage quals Tdouble

    go [TSlong, TSdouble] = do
        checkNoSign specs "sign specified for long double type"
        return $ DeclSpec storage quals Tlong_double

    go [TSdouble, TSlong] = do
        checkNoSign specs "sign specified for long double type"
        return $ DeclSpec storage quals Tlong_double

    go [TSstruct id fields attrs] = do
        checkNoSign specs "sign specified for struct type"
        return $ DeclSpec storage quals (Tstruct id fields attrs)

    go [TSunion id fields attrs] = do
        checkNoSign specs "sign specified for union type"
        return $ DeclSpec storage quals (Tunion id fields attrs)

    go [TSenum id enums attrs] = do
        checkNoSign specs "sign specified for enum type"
        return $ DeclSpec storage quals (Tenum id enums attrs)

    go [TSnamed id] = do
        checkNoSign specs "sign specified for named type"
        return $ DeclSpec storage quals (Tnamed id)

    go [TSva_list] = do
        checkNoSign specs "sign specified for __builtin_va_list"
        return $ DeclSpec storage quals Tva_list

    go [] = do
        sign <- mkSign specs
        return $ DeclSpec storage quals (Tint sign)

    go tyspecs = throwException (BadType $ spread (map ppr tyspecs))

mkPtr :: [TySpec] -> Decl -> Decl
mkPtr quals decl = C.Ptr (mkTypeQuals quals) decl

mkArray :: Maybe Exp -> Decl -> Decl
mkArray dim decl = Array decl dim

mkProto :: Params -> Decl -> Decl
mkProto args decl = Proto decl args

mkOldProto :: [Id] -> Decl -> Decl
mkOldProto args decl = OldProto decl args

checkInitGroup :: InitGroup -> P InitGroup
checkInitGroup group@(InitGroup (DeclSpec storage quals spec) attrs inits)
    | any (== Ttypedef) storage = do
          typedefs <- mapM go inits
          return $ TypedefGroup (DeclSpec storage' quals spec) attrs typedefs
  where
    storage' :: [Storage]
    storage' = [x | x <- storage, x /= Ttypedef]

    go :: Init -> P Typedef
    go (Init id _  (Just _) _)=
        throwException (TypedefInitialized id)

    go (Init id@(Id name) decl _ attrs) = do
        addTypedef name
        return (Typedef id decl attrs)

    go (Init id@(AntiId _) decl _ attrs) =
        return (Typedef id decl attrs)

checkInitGroup group@(InitGroup _ _ inits) = do
    mapM_ go inits
    return group
  where
    go :: Init -> P ()
    go (Init id@(Id name) _ _ _)  = addVariable name
    go (Init id@(AntiId _) _ _ _) = return ()
}
