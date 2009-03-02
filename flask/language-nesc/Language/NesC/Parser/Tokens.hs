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
-- Module      :  Language.NesC.Parser.Tokens
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.NesC.Parser.Tokens (
    Token(..),
    keywords,
    gccKeywords,
    nescKeywords
  ) where

import Data.Maybe (fromMaybe)

data Token = Teof
           | TcharConst Char
           | TstringConst String
             -- Bool flag is True of the constant is unsigned
           | TintConst (String, Bool, Integer)
           | TlongIntConst (String, Bool, Integer)
           | TlongLongIntConst (String, Bool, Integer)
           | TfloatConst (String, Double)
           | TdoubleConst (String, Double)
           | TlongDoubleConst (String, Double)
           | Tidentifier String
           | Tnamed String
           | Tlparen
           | Trparen
           | Tlbrack
           | Trbrack
           | Tlbrace
           | Trbrace
           | Tcomma
           | Tsemi
           | Tcolon
           | Tquestion
           | Tdot
           | Tarrow
           | Tellipses

           | Tplus
           | Tminus
           | Tstar
           | Tdiv
           | Tmod
           | Tnot
           | Tand
           | Tor
           | Txor
           | Tlsh
           | Trsh
           | Tinc
           | Tdec

           | Tlnot
           | Tland
           | Tlor

           | Teq
           | Tne
           | Tlt
           | Tgt
           | Tle
           | Tge

           | Tassign
           | Tadd_assign
           | Tsub_assign
           | Tmul_assign
           | Tdiv_assign
           | Tmod_assign
           | Tlsh_assign
           | Trsh_assign
           | Tand_assign
           | Tor_assign
           | Txor_assign

           | Tauto
           | Tbreak
           | Tcase
           | Tchar
           | Tconst
           | Tcontinue
           | Tdefault
           | Tdo
           | Tdouble
           | Telse
           | Tenum
           | Textern
           | Tfloat
           | Tfor
           | Tgoto
           | Tif
           | Tint
           | Tlong
           | Tregister
           | Treturn
           | Tshort
           | Tsigned
           | Tsizeof
           | Tstatic
           | Tstruct
           | Tswitch
           | Ttypedef
           | Tunion
           | Tunsigned
           | Tvoid
           | Tvolatile
           | Twhile

           | Tasm
           | Tattribute
           | Tbuiltin_va_arg
           | Tbuiltin_va_list
           | Tinline

           | Ttypename

           | Tas
           | Tatomic
           | Tasync
           | Tcall
           | Tcommand
           | Tcomponents
           | Tconfiguration
           | Tevent
           | Tgeneric
           | Timplementation
           | Tincludes
           | Tinterface
           | Tmodule
           | Tnew
           | Tnorace
           | Tpost
           | Tprovides
           | Tsignal
           | Ttask
           | Tuses
           | Tabstract
           | Tcomponent
           | Textends
           | Tleft_arrow

           | Tanti_id String
           | Tanti_int String
           | Tanti_uint String
           | Tanti_lint String
           | Tanti_ulint String
           | Tanti_float String
           | Tanti_double String
           | Tanti_long_double String
           | Tanti_char String
           | Tanti_string String
           | Tanti_exp String
           | Tanti_func String
           | Tanti_args String
           | Tanti_decl String
           | Tanti_decls String
           | Tanti_sdecl String
           | Tanti_sdecls String
           | Tanti_enum String
           | Tanti_enums String
           | Tanti_edecl String
           | Tanti_edecls String
           | Tanti_stm String
           | Tanti_stms String
           | Tanti_type String
           | Tanti_param String
           | Tanti_params String

           | Tanti_uses_provides String
           | Tanti_uses_provides_list String
           | Tanti_components String
           | Tanti_components_list String
           | Tanti_connection String
           | Tanti_connections String
    deriving (Ord, Eq)

instance Show Token where
    show Teof                           = "EOF"
    show (TcharConst c)                 = show c
    show (TstringConst s)               = show s
    show (TintConst (s, _, _))          = s
    show (TlongIntConst (s, _, _))      = s
    show (TlongLongIntConst (s, _, _))  = s
    show (TfloatConst (s, _))           = s
    show (TdoubleConst (s, _))          = s
    show (TlongDoubleConst (s, _))      = s
    show (Tidentifier s)                = "identifier: " ++ show s
    show t = fromMaybe (error "internal error: unknown token")
                       (lookup t tokenStrings)

tokenStrings :: [(Token, String)]
tokenStrings = [(Tlparen,     "("),
                (Trparen,     ")"),
                (Tlbrack,     "["),
                (Trbrack,     "]"),
                (Tlbrace,     "{"),
                (Trbrace,     "}"),
                (Tcomma,      ","),
                (Tsemi,       ";"),
                (Tcolon,      ":"),
                (Tquestion,   "?"),
                (Tdot,        "."),
                (Tarrow,      "->"),
                (Tellipses,   "..."),
                (Tplus,       "+"),
                (Tminus,      "-"),
                (Tstar,       "*"),
                (Tdiv,        "/"),
                (Tmod,        "%"),
                (Tnot,        "~"),
                (Tand,        "&"),
                (Tor,         "|"),
                (Txor,        "^"),
                (Tlsh,        "<<"),
                (Trsh,        ">>"),
                (Tinc,        "++"),
                (Tdec,        "--"),
                (Tlnot,       "!"),
                (Tland,       "&&"),
                (Tlor,        "||"),
                (Teq,         "=="),
                (Tne,         "!="),
                (Tlt,         "<"),
                (Tgt,         ">"),
                (Tle,         "<="),
                (Tge,         ">="),
                (Tassign,     "="),
                (Tadd_assign, "+="),
                (Tsub_assign, "-="),
                (Tmul_assign, "*="),
                (Tdiv_assign, "/="),
                (Tmod_assign, "%="),
                (Tlsh_assign, "<<="),
                (Trsh_assign, ">>="),
                (Tand_assign, "&="),
                (Tor_assign,  "|="),
                (Txor_assign, "^="),

                --
                -- Keywords
                --
                (Tauto,     "auto"),
                (Tbreak,    "break"),
                (Tcase,     "case"),
                (Tchar,     "char"),
                (Tconst,    "const"),
                (Tcontinue, "continue"),
                (Tdefault,  "default"),
                (Tdo,       "do"),
                (Tdouble,   "double"),
                (Telse,     "else"),
                (Tenum,     "enum"),
                (Textern,   "extern"),
                (Tfloat,    "float"),
                (Tfor,      "for"),
                (Tgoto,     "goto"),
                (Tif,       "if"),
                (Tint,      "int"),
                (Tlong,     "long"),
                (Tregister, "register"),
                (Treturn,   "return"),
                (Tshort,    "short"),
                (Tsigned,   "signed"),
                (Tsizeof,   "sizeof"),
                (Tstatic,   "static"),
                (Tstruct,   "struct"),
                (Tswitch,   "switch"),
                (Ttypedef,  "typedef"),
                (Tunion,    "union"),
                (Tunsigned, "unsigned"),
                (Tvoid,     "void"),
                (Tvolatile, "volatile"),
                (Twhile,    "while"),

                --
                -- GCC extensions
                --
                (Tasm,             "__asm__"),
                (Tattribute,       "__attribute__"),
                (Tbuiltin_va_arg,  "__builtin_va_arg"),
                (Tbuiltin_va_list, "__builtin_va_list"),
                (Tinline,          "__inline__"),

                --
                -- NesC keywords
                --
                (Tleft_arrow,     "<-"),

                (Tas,             "as"),
                (Tatomic,         "atomic"),
                (Tasync,          "async"),
                (Tcall,           "call"),
                (Tcommand,        "command"),
                (Tcomponents,     "components"),
                (Tconfiguration,  "configuration"),
                (Tevent,          "event"),
                (Tgeneric,        "generic"),
                (Timplementation, "implementation"),
                (Tincludes,       "includes"),
                (Tinterface,      "interface"),
                (Tmodule,         "module"),
                (Tnew,            "new"),
                (Tnorace,         "norace"),
                (Tpost,           "post"),
                (Tprovides,       "provides"),
                (Tsignal,         "signal"),
                (Ttask,           "task"),
                (Tuses,           "uses"),
                (Tabstract,       "abstract"),
                (Tcomponent,      "component"),
                (Textends,        "extends")
                ]

keywords :: [(String,    Token)]
keywords = [("auto",     Tauto),
            ("break",    Tbreak),
            ("case",     Tcase),
            ("char",     Tchar),
            ("const",    Tconst),
            ("continue", Tcontinue),
            ("default",  Tdefault),
            ("do",       Tdo),
            ("double",   Tdouble),
            ("else",     Telse),
            ("enum",     Tenum),
            ("extern",   Textern),
            ("float",    Tfloat),
            ("for",      Tfor),
            ("goto",     Tgoto),
            ("if",       Tif),
            ("int",      Tint),
            ("long",     Tlong),
            ("register", Tregister),
            ("return",   Treturn),
            ("short",    Tshort),
            ("signed",   Tsigned),
            ("sizeof",   Tsizeof),
            ("static",   Tstatic),
            ("struct",   Tstruct),
            ("switch",   Tswitch),
            ("typedef",  Ttypedef),
            ("union",    Tunion),
            ("unsigned", Tunsigned),
            ("void",     Tvoid),
            ("volatile", Tvolatile),
            ("while",    Twhile)
           ]

gccKeywords :: [(String,    Token)]
gccKeywords = [("asm",               Tasm),
               ("__asm",             Tasm),
               ("__asm__",           Tasm),
               ("__attribute__",     Tattribute),
               ("__builtin_va_arg",  Tbuiltin_va_arg),
               ("__builtin_va_list", Tbuiltin_va_list),
               ("inline",            Tinline),
               ("__inline",          Tinline),
               ("__inline__",        Tinline)
              ]

nescKeywords :: [(String,    Token)]
nescKeywords = [("as",             Tas),
                ("atomic",         Tatomic),
                ("async",          Tasync),
                ("call",           Tcall),
                ("command",        Tcommand),
                ("components",     Tcomponents),
                ("configuration",  Tconfiguration),
                ("event",          Tevent),
                ("generic",        Tgeneric),
                ("implementation", Timplementation),
                ("includes",       Tincludes),
                ("interface",      Tinterface),
                ("module",         Tmodule),
                ("new",            Tnew),
                ("norace",         Tnorace),
                ("post",           Tpost),
                ("provides",       Tprovides),
                ("signal",         Tsignal),
                ("task",           Ttask),
                ("uses",           Tuses),
                ("abstract",       Tabstract),
                ("component",      Tcomponent),
                ("extends",        Textends)
               ]
