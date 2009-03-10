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
-- Module      :  Language.Hs.Parser.Tokens
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Parser.Tokens where

import Data.Maybe (fromJust)

data Token  =  Tcase
            |  Tclass
            |  Tdata
            |  Tdefault
            |  Tderiving
            |  Tdo
            |  Telse
            |  Tif
            |  Timport
            |  Tin
            |  Tinfix
            |  Tinfixl
            |  Tinfixr
            |  Tinstance
            |  Tlet
            |  Tmodule
            |  Tnewtype
            |  Tof
            |  Tthen
            |  Ttype
            |  Twhere
            |  Tunderscore

            |  Tas
            |  Tqualified
            |  Thiding

            |  Tdotdot
            |  Tcolon
            |  Tdcolon
            |  Tequal
            |  Tlam
            |  Tvbar
            |  Tlarrow
            |  Trarrow
            |  Tat
            |  Ttilda
            |  Tdarrow
            |  Tminus
            |  Tbang

            |  Tlparen
            |  Trparen
            |  Tcomma
            |  Tsemi
            |  Tlbrack
            |  Trbrack
            |  Tbackquote
            |  Tlbrace
            |  Trbrace

            |  Tinteger Integer
            |  Tfloat Double
            |  Tchar Char
            |  Tstring String

            |  Tws String
            |  Tindent Int
            |  Timplbrace Int
            |  Timprbrace

            |  Tqvarid [String] String
            |  Tqconid [String] String
            |  Tqvarsym [String] String
            |  Tqconsym [String] String

            |  Teof

            |  Tanti_id String
            |  Tanti_int String
            |  Tanti_float String
            |  Tanti_var String
            |  Tanti_type String
            |  Tanti_exp String
    deriving (Ord, Eq)

instance Show Token where
    show (Tinteger n)          =  show n
    show (Tfloat n)            =  show n
    show (Tchar c)             =  show c
    show (Tstring s)           =  show s
    show (Tws _)               =  "whitespace"
    show (Tindent n)           =  "indentation " ++ show n
    show (Timplbrace n)        =  "implicit { " ++ show n
    show Timprbrace            =  "implicit }"
    show (Tqvarid mods name)   =  describeQualified mods $
                                  "variable identifier " ++ name
    show (Tqconid mods name)   =  describeQualified mods $
                                  "constructor identifier " ++ name
    show (Tqvarsym mods name)  =  describeQualified mods $
                                  "variable symbol " ++ name
    show (Tqconsym mods name)  =  describeQualified mods $
                                  "constructor symbol " ++ name
    show Teof                  =  "EOF"
    show x                     =  fromJust (lookup x tokenStrings)

describeQualified [] s = s
describeQualified _  s = "qualified " ++ s

tokenStrings :: [(Token, String)]
tokenStrings = [(Tcase,        "case"),
                (Tclass,       "class"),
                (Tdata,        "data"),
                (Tdefault,     "default"),
                (Tderiving,    "deriving"),
                (Tdo,          "do"),
                (Telse,        "else"),
                (Tif,          "if"),
                (Timport,      "import"),
                (Tin,          "in"),
                (Tinfix,       "infix"),
                (Tinfixl,      "infixl"),
                (Tinfixr,      "infixr"),
                (Tinstance,    "instance"),
                (Tlet,         "let"),
                (Tmodule,      "module"),
                (Tnewtype,     "newtype"),
                (Tof,          "of"),
                (Tthen,        "then"),
                (Ttype,        "type"),
                (Twhere,       "where"),
                (Tunderscore,  "_"),

                (Tas,         "as"),
                (Tqualified,  "qualified"),
                (Thiding,     "hiding"),

                (Tdotdot,  ".."),
                (Tcolon,   ":"),
                (Tdcolon,  "::"),
                (Tequal,   "="),
                (Tlam,     "\\"),
                (Tvbar,    "|"),
                (Tlarrow,  "<-"),
                (Trarrow,  "->"),
                (Tat,      "@"),
                (Ttilda,   "~"),
                (Tdarrow,  "=>"),
                (Tminus,   "-"),
                (Tbang,    "!"),

                (Tlparen,     "("),
                (Trparen,     ")"),
                (Tcomma,      ","),
                (Tsemi,       ";"),
                (Tlbrack,     "["),
                (Trbrack,     "]"),
                (Tbackquote,  "`"),
                (Tlbrace,     "{"),
                (Trbrace,     "}")
               ]
\end{code}
