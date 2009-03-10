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
-- Module      :  Language.F.Parser.Tokens
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.F.Parser.Tokens where

import Data.Maybe (fromJust)

data Token  =  Taxiom
            |  Tcase
            |  Tdata
            |  Timport
            |  Tin
            |  Tleft
            |  Tlet
            |  Tletrec
            |  Tmodule
            |  Tof
            |  Tright
            |  Tsym
            |  Ttype
            |  Twhere
            |  Tunderscore

            |  Tdot
            |  Tcolon
            |  Tdcolon
            |  Tequal
            |  Tlam
            |  TLam
            |  Trarrow
            |  Tdarrow
            |  Tbang
            |  Tstar
            |  Tforall

            |  Tcosim
            |  Tcotrans
            |  Tcoapp
            |  Tcast

            |  Tlparen
            |  Trparen
            |  Tcomma
            |  Tsemi
            |  Tlbrack
            |  Trbrack
            |  Tlbrace
            |  Trbrace

            |  Tinteger Integer
            |  Tfloat Double
            |  Tchar Char
            |  Tstring String

            |  Tws String

            |  Tqvarid [String] String
            |  Tqconid [String] String
            |  Tqvarsym [String] String
            |  Tqconsym [String] String

            |  Teof
    deriving (Ord, Eq)

instance Show Token where
    show (Tinteger n)          =  show n
    show (Tfloat n)            =  show n
    show (Tchar c)             =  show c
    show (Tstring s)           =  show s
    show (Tws _)               =  "whitespace"
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

tokenStrings :: [(Token, String)]
tokenStrings = [(Taxiom,       "axiom"),
                (Tcase,        "case"),
                (Tdata,        "data"),
                (Timport,      "import"),
                (Tin,          "in"),
                (Tleft,        "left"),
                (Tlet,         "let"),
                (Tmodule,      "module"),
                (Tof,          "of"),
                (Tright,       "right"),
                (Tsym,         "sym"),
                (Ttype,        "type"),
                (Twhere,       "where"),

                (Tdot,      "."),
                (Tcolon,    ":"),
                (Tdcolon,   "::"),
                (Tequal,    "="),
                (Tlam,      "\\"),
                (TLam,      "/\\"),
                (Trarrow,   "->"),
                (Tdarrow,   "=>"),
                (Tbang,     "!"),
                (Tstar,     "*"),
                (Tforall,   "\\/"),

                (Tcosim,    "~"),
                (Tcotrans,  "o"),
                (Tcoapp,    "@"),
                (Tcast,     "|>"),

                (Tlparen,     "("),
                (Trparen,     ")"),
                (Tcomma,      ","),
                (Tsemi,       ";"),
                (Tlbrack,     "["),
                (Trbrack,     "]"),
                (Tlbrace,     "{"),
                (Trbrace,     "}")
               ]

describeQualified [] s = s
describeQualified _  s = "qualified " ++ s
\end{code}
