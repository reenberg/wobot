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
-- Module      :  Language.NesC.Parser.Lexer
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.NesC.Parser.Lexer (
    lexToken,
    exp,
    pat
  ) where

import Prelude hiding(exp)

import Control.Monad (when)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, isOctDigit, isHexDigit, isSpace, chr, toLower)
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Maybe (fromMaybe)

import Control.Monad.ContextException
import Data.Loc
import qualified Language.NesC.Syntax as C
import Language.NesC.Parser.Exceptions
import Language.NesC.Parser.Monad
import Language.NesC.Parser.Tokens
import Text.PrettyPrint.Mainland
}

$nondigit         = [a-z A-Z \_]
$digit            = [0-9]
$nonzerodigit     = [1-9]
$octalDigit       = [0-7]
$hexadecimalDigit = [0-9A-Fa-f]

@fractionalConstant = $digit* "." $digit+
                    | $digit+ "."
@exponentPart       = [eE] [\+\-]? $digit+
@floatingSuffix     = [lLfF]
@floatingConstant   = @fractionalConstant @exponentPart? @floatingSuffix?
                    | $digit+ @exponentPart @floatingSuffix?

@decimalConstant     = $nonzerodigit $digit*
@octalConstant       = "0" $octalDigit*
@hexadecimalConstant = "0" [xX] $hexadecimalDigit+

@integerSuffix = [uU] [lL]?
               | [lL] [uU]?
               | ("ll" | "LL")

$whitechar = [\ \t\n\r\f\v]

$hssmall  = [a-z_]
$hsidchar = [a-zA-Z0-9_\']
@hsvarid  = $hssmall $hsidchar*

nesc :-

<exp, pat> {
 "typename"            / { allowAnti } { token Ttypename }

 "$id:"      @hsvarid  / { allowAnti } { anti Tanti_id }
 "$int:"     @hsvarid  / { allowAnti } { anti Tanti_int }
 "$uint:"    @hsvarid  / { allowAnti } { anti Tanti_uint }
 "$lint:"    @hsvarid  / { allowAnti } { anti Tanti_lint }
 "$ulint:"   @hsvarid  / { allowAnti } { anti Tanti_ulint }
 "$float:"   @hsvarid  / { allowAnti } { anti Tanti_float }
 "$double:"  @hsvarid  / { allowAnti } { anti Tanti_double }
 "$ldouble:" @hsvarid  / { allowAnti } { anti Tanti_long_double }
 "$char:"    @hsvarid  / { allowAnti } { anti Tanti_char }
 "$string:"  @hsvarid  / { allowAnti } { anti Tanti_string }
 "$exp:"     @hsvarid  / { allowAnti } { anti Tanti_exp }
 "$func:"    @hsvarid  / { allowAnti } { anti Tanti_func }
 "$args:"    @hsvarid  / { allowAnti } { anti Tanti_args }
 "$decl:"    @hsvarid  / { allowAnti } { anti Tanti_decl }
 "$decls:"   @hsvarid  / { allowAnti } { anti Tanti_decls }
 "$sdecl:"   @hsvarid  / { allowAnti } { anti Tanti_sdecl }
 "$sdecls:"  @hsvarid  / { allowAnti } { anti Tanti_sdecls }
 "$enum:"    @hsvarid  / { allowAnti } { anti Tanti_enum }
 "$enums:"   @hsvarid  / { allowAnti } { anti Tanti_enums }
 "$edecl:"   @hsvarid  / { allowAnti } { anti Tanti_edecl }
 "$edecls:"  @hsvarid  / { allowAnti } { anti Tanti_edecls }
 "$stm:"     @hsvarid  / { allowAnti } { anti Tanti_stm }
 "$stms:"    @hsvarid  / { allowAnti } { anti Tanti_stms }
 "$ty:"      @hsvarid  / { allowAnti } { anti Tanti_type }
 "$param:"   @hsvarid  / { allowAnti } { anti Tanti_param }
 "$params:"  @hsvarid  / { allowAnti } { anti Tanti_params }

 "$usesprovides:"  @hsvarid  / { allowNesCAnti } { anti Tanti_uses_provides }
 "$usesprovidesl:" @hsvarid  / { allowNesCAnti } { anti Tanti_uses_provides_list }
 "$components:"    @hsvarid  / { allowNesCAnti } { anti Tanti_components }
 "$componentsl:"   @hsvarid  / { allowNesCAnti } { anti Tanti_components_list }
 "$connection:"    @hsvarid  / { allowNesCAnti } { anti Tanti_connection }
 "$connections:"   @hsvarid  / { allowNesCAnti } { anti Tanti_connections }
}

<0, exp, pat> {
 "//" .* ;
 "/*" ([^\*]|[\r\n]|("*"+([^\*\/]|[\r\n])))* "*"+ "/" ;

 ^ "#" .*        ;
 $whitechar+     ;
 "__extension__" ;

 $nondigit ($nondigit | $digit)* { identifier }

 @floatingConstant                    { lex_float }
 @decimalConstant @integerSuffix?     { lex_dec_int }
 @octalConstant @integerSuffix?       { lex_oct_int }
 @hexadecimalConstant @integerSuffix? { lex_hex_int }

 \' { lex_char_tok }
 \" { lex_string_tok }

 "("   { token Tlparen }
 ")"   { token Trparen }
 "["   { token Tlbrack }
 "]"   { token Trbrack }
 "{"   { token Tlbrace }
 "}"   { token Trbrace }
 ","   { token Tcomma }
 ";"   { token Tsemi }
 ":"   { token Tcolon }
 "?"   { token Tquestion }
 "."   { token Tdot }
 "->"  { token Tarrow }
 "..." { token Tellipses }

 "+"  { token Tplus }
 "-"  { token Tminus }
 "*"  { token Tstar }
 "/"  { token Tdiv }
 "%"  { token Tmod }
 "~"  { token Tnot }
 "&"  { token Tand }
 "|"  { token Tor }
 "^"  { token Txor }
 "<<" { token Tlsh }
 ">>" { token Trsh }
 "++" { token Tinc }
 "--" { token Tdec }

 "!"  { token Tlnot }
 "&&" { token Tland }
 "||" { token Tlor }

 "==" { token Teq }
 "!=" { token Tne }
 "<"  { token Tlt }
 ">"  { token Tgt }
 "<=" { token Tle }
 ">=" { token Tge }

 "="   { token Tassign }
 "+="  { token Tadd_assign }
 "-="  { token Tsub_assign }
 "*="  { token Tmul_assign }
 "/="  { token Tdiv_assign }
 "%="  { token Tmod_assign }
 "&="  { token Tand_assign }
 "|="  { token Tor_assign }
 "^="  { token Txor_assign }
 "<<=" { token Tlsh_assign }
 ">>=" { token Trsh_assign }

 "<-"  / { allowNesC } { token Tleft_arrow }
}

{
charEscapes :: [(Char, Char)]
charEscapes = [('n', '\n'),
               ('t', '\t'),
               ('v', '\v'),
               ('b', '\b'),
               ('r', '\r'),
               ('f', '\f'),
               ('a', '\a'),
               ('a', '\a'),
               ('\\', '\\'),
               ('?', '?'),
               ('\'', '\''),
               ('\"', '\"')
              ]

type Action = Pos -> B.ByteString -> Int -> P (L Token)

locate :: Pos -> Token -> P (L Token)
locate end tok =
    do  start <- getLastPos
        setLastPos end
        return $ L (Loc start end) tok

token :: Token -> Action
token tok pos buf len = locate pos tok

allowGcc  ::  PEnv       -- predicate state
          ->  AlexInput  -- input stream before the token
          ->  Int        -- length of the token
          ->  AlexInput  -- input stream after the token
          ->  Bool
allowGcc (PEnv { gcc=flag }) _ _ _ = flag

allowNesc  ::  PEnv       -- predicate state
           ->  AlexInput  -- input stream before the token
           ->  Int        -- length of the token
           ->  AlexInput  -- input stream after the token
           ->  Bool
allowNesc (PEnv { nesc=flag }) _ _ _ = flag

allowAnti  ::  PEnv       -- predicate state
           ->  AlexInput  -- input stream before the token
           ->  Int        -- length of the token
           ->  AlexInput  -- input stream after the token
           ->  Bool
allowAnti  (PEnv { context=ParseExpression })  _ _ _  = True
allowAnti  (PEnv { context=ParsePattern })     _ _ _  = True
allowAnti  _                                   _ _ _  = False

allowNesC  ::  PEnv       -- predicate state
           ->  AlexInput  -- input stream before the token
           ->  Int        -- length of the token
           ->  AlexInput  -- input stream after the token
           ->  Bool
allowNesC  (PEnv { nesc = flag }) _ _ _ = flag

allowNesCAnti  ::  PEnv       -- predicate state
               ->  AlexInput  -- input stream before the token
               ->  Int        -- length of the token
               ->  AlexInput  -- input stream after the token
               ->  Bool
allowNesCAnti  (PEnv { context = ParseExpression, nesc = flag }) _ _ _ = flag
allowNesCAnti  (PEnv { context = ParsePattern, nesc = flag })    _ _ _ = flag
allowNesCAnti  _                                                 _ _ _ = False

anti :: (String -> Token) -> Action
anti antiTok pos buf len =
    do let s = B.tail $ B.dropWhile ((/=) ':') $ B.take len buf
       locate pos (antiTok $ B.unpack s)

lex_dec_int :: Action
lex_dec_int pos buf len
    = do  let s = B.unpack $ B.take len buf
          readInt "" s 10 digit

lex_oct_int :: Action
lex_oct_int pos buf len
    = do  let (o : s) = B.unpack $ B.take len buf
          readInt (o : []) s 8 octDigit

lex_hex_int :: Action
lex_hex_int pos buf len
    = do  let (o : x : s) = B.unpack $ B.take len buf
          readInt (o : x : []) s 16 hexDigit

lex_float :: Action
lex_float pos buf len =
    do  let s = B.unpack $ B.take len buf
        -- read can't handle a leading or trailing '.'
        let prefix = takeWhile (not . (`elem` ['l', 'L', 'f', 'F'])) s
        let suffix = takeWhile (`elem` ['l', 'L', 'f', 'F', '.']) $ reverse s
        let cleanPrefix = reverse $ dropWhile (== '.') (reverse prefix)
        let n = case cleanPrefix of
                  '.' : _  -> read ('0' : cleanPrefix)
                  _        -> read cleanPrefix
        if ('f' `elem` suffix || 'F' `elem` suffix)
          then locate pos $ TfloatConst (s, n)
          else if ('l' `elem` suffix || 'L' `elem` suffix)
                 then locate pos $ TlongDoubleConst (s, n)
                 else locate pos $ TdoubleConst (s, n)

identifier :: Action
identifier pos buf len
    = do  let kw = B.unpack $ B.take len buf
          keywords <- getKeywords
          case Map.lookup kw keywords of
            Nothing   -> do  test <- isTypedef kw
                             if test
                               then locate pos (Tnamed kw)
                               else locate pos (Tidentifier kw)
            Just tok  -> locate pos tok

lex_char_tok :: Action
lex_char_tok pos buf len =
    do  input <- getInput
        (c, input) <- alexGetCharOrFail input
        case c of
          '\''  -> throwExceptionAt pos EmptyCharacterLiteral
          '\\'  -> do  setInput input
                       c <- lex_escape
                       input <- getInput
                       (quote, input) <- alexGetCharOrFail input
                       when (quote /= '\'') $
                            throwExceptionAt pos IllegalCharacterLiteral
                       setInput input
                       end <- getPos
                       locate end (TcharConst c)
          _ ->     do  (quote, input) <- alexGetCharOrFail input
                       when (quote /= '\'') $
                            throwExceptionAt pos IllegalCharacterLiteral
                       setInput input
                       end <- getPos
                       locate end (TcharConst c)

lex_string_tok :: Action
lex_string_tok pos buf len =
    do  s <- lex_string ""
        end <- getPos
        locate end (TstringConst (reverse s))
  where
    lex_string :: String -> P String
    lex_string s =
        do  input <- getInput
            (c, i1) <- alexGetCharOrFail input
            case c of
              '"'   -> do  setInput i1
                           return s
              '\\'  -> do  setInput i1
                           c <- lex_escape
                           lex_string (c : s)
              _     -> do  setInput i1
                           lex_string (c : s)

lex_escape :: P Char
lex_escape =
    do  input <- getInput
        (c, i1) <- alexGetCharOrFail input
        case c of
          'x' -> do  setInput i1
                     i <- checkedReadNum isHexDigit 16 hexDigit
                     return $ chr i
          n | isDigit n ->
                  do  i <- checkedReadNum isOctDigit 8 octDigit
                      return $ chr i
          c -> case (lookup c charEscapes) of
                 Nothing -> do  setInput i1
                                return c
                 Just c' -> do  setInput i1
                                return c'

digit :: Char -> Int
digit c | c >= '0' && c <= '9' = ord c - ord '0'
        | otherwise            = error "error in decimal constant"

octDigit :: Char -> Int
octDigit c | c >= '0' && c <= '7' = ord c - ord '0'
           | otherwise            = error "error in octal constant"

hexDigit :: Char -> Int
hexDigit c | c >= 'a' && c <= 'f' = 10 + ord c - ord 'a'
           | c >= 'A' && c <= 'F' = 10 + ord c - ord 'A'
           | c >= '0' && c <= '9' = ord c - ord '0'
           | otherwise            = error "error in hexadecimal constant"

checkedReadNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
checkedReadNum isDigit base conv =
    do  inp@(AlexInput pos _ _) <- getInput
        (c, _) <- alexGetCharOrFail inp
        when (not $ isDigit c) $
             throwExceptionAt pos IllegalNumericLiteral
        readNum isDigit base conv

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
readNum isDigit base conv =
    do  input <- getInput;
        read 0 input
    where
      read :: Int -> AlexInput -> P Int
      read i input =
          do  (c, input') <- alexGetCharOrFail input
              if isDigit c
                then do  let n = conv c
                         read (i*base + n) input'
                else do  setInput input
                         return i

readInt :: String -> String -> Int -> (Char -> Int) -> P (L Token)
readInt prefix s base conv =
    do  let b = toInteger base
        let num = takeWhile (not . (`elem` ['l', 'L', 'u', 'U'])) s
        let suffix = map toLower $ takeWhile (`elem` ['l', 'L', 'u', 'U']) $ reverse s
        let isUnsigned = 'u' `elem` suffix
        let lSuffix = takeWhile ((==) 'l') suffix
        let n = foldl' (\n c -> n*b + (toInteger $ conv c)) (toInteger 0) num
        end <- getPos
        locate end $
          case lSuffix of
            []    -> TintConst (prefix ++ s, isUnsigned, n)
            "l"   -> TlongIntConst (prefix ++ s, isUnsigned, n)
            "ll"  -> TlongLongIntConst (prefix ++ s, isUnsigned, n)

lexToken :: P (L Token)
lexToken = do
  inp@(AlexInput pos buf off) <- getInput
  sc <- getLexState
  st <- getPEnv
  case alexScanUser st inp sc of
    AlexEOF -> return $ L (Loc pos pos) Teof
    AlexError (AlexInput pos2 _ _) ->
        throwExceptionAt pos2 $
        LexerError $ text $
        B.unpack $ B.take (min 80 (B.length rest)) rest
      where
        rest = B.drop off buf
    AlexSkip inp2 _ -> do
        setInput inp2
        pos <- getPos
        setLastPos pos
        lexToken
    AlexToken inp2@(AlexInput end _ _) len t -> do
        setInput inp2
        t end (B.drop off buf) len
}
