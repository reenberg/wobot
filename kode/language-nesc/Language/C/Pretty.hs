{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

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
-- Module      :  Language.C.Pretty
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.C.Pretty where

import List (intersperse)
import Char (isPrint, ord)
import Numeric (showHex)

import Language.C.Syntax
import Text.PrettyPrint.Mainland

class CFixity a where
    fixity :: a -> OpFixity

instance CFixity BinOp where
    fixity Add  = (LeftAssoc, 12)
    fixity Sub  = (LeftAssoc, 12)
    fixity Mul  = (LeftAssoc, 13)
    fixity Div  = (LeftAssoc, 13)
    fixity Mod  = (LeftAssoc, 13)
    fixity Eq   = (LeftAssoc, 9)
    fixity Ne   = (LeftAssoc, 9)
    fixity Lt   = (LeftAssoc, 10)
    fixity Gt   = (LeftAssoc, 10)
    fixity Le   = (LeftAssoc, 10)
    fixity Ge   = (LeftAssoc, 10)
    fixity Land = (LeftAssoc, 5)
    fixity Lor  = (LeftAssoc, 4)
    fixity And  = (LeftAssoc, 8)
    fixity Or   = (LeftAssoc, 6)
    fixity Xor  = (LeftAssoc, 7)
    fixity Lsh  = (LeftAssoc, 11)
    fixity Rsh  = (LeftAssoc, 11)

instance CFixity AssignOp where
    fixity _ = (RightAssoc, 2)

instance CFixity UnOp where
    fixity _ = (RightAssoc, 14)

instance Pretty Id where
    ppr (Id ident)  = text ident
    ppr (AntiId v)  = ppr "$id:" <> ppr v

instance Pretty Storage where
    ppr Tauto     = text "auto"
    ppr Tregister = text "register"
    ppr Tstatic   = text "static"
    ppr Textern   = text "extern"
    ppr Ttypedef  = text "typedef"

instance Pretty TypeQual where
    ppr Tconst    = text "const"
    ppr Tvolatile = text "volatile"
    ppr Tinline   = text "inline"

instance Pretty Sign where
    ppr Tsigned   = text "signed"
    ppr Tunsigned = text "unsigned"

instance Pretty TypeSpec where
    ppr Tvoid             = text "void"
    ppr (Tchar sign)      = ppr sign <+> text "char"
    ppr (Tshort sign)     = ppr sign <+> text "short"
    ppr (Tint sign)       = ppr sign <+> text "int"
    ppr (Tlong sign)      = ppr sign <+> text "long"
    ppr (Tlong_long sign) = ppr sign <+> text "long" <+> text "long"
    ppr Tfloat            = text "float"
    ppr Tdouble           = text "double"
    ppr Tlong_double      = text "long" <+> text "double"

    ppr (Tstruct maybe_id maybe_fields attrs) =
        pprStructOrUnion "struct" maybe_id maybe_fields attrs

    ppr (Tunion maybe_id maybe_fields attrs) =
        pprStructOrUnion "union" maybe_id maybe_fields attrs

    ppr (Tenum maybe_id cenums attrs) =
        pprEnum maybe_id cenums attrs

    ppr (Tnamed ident) =
        ppr ident

    ppr Tva_list =
        text "__builtin_va_list"

embracelines s xs =
    nest 4 (text "{" </>
         seplines s (map (nest 4 . ppr) xs)) <> s
    </> text "}"

pprStructOrUnion :: String
                 -> Maybe Id
                 -> Maybe [FieldGroup]
                 -> [Attr]
                 -> Doc
pprStructOrUnion ty maybe_id maybe_fields attrs =
    text ty <+/> ppr maybe_id
    <+/> case maybe_fields of
           Nothing     -> empty
           Just fields -> embracelines semi fields
    <+> ppr attrs

pprEnum :: Maybe Id
        -> [CEnum]
        -> [Attr]
        -> Doc
pprEnum maybe_id cenums attrs =
    text "enum" <+/> ppr maybe_id
    <+/> case cenums of
           [] -> empty
           _  -> embracelines comma cenums
    <+> ppr attrs

instance Pretty DeclSpec where
    ppr (DeclSpec storage quals spec) =
        spread (map ppr storage ++ map ppr quals) <+/>
        ppr spec

    ppr (AntiTypeDeclSpec storage quals v) =
        spread (map ppr storage ++ map ppr quals) <+/>
        ppr "$ty:" <> ppr v

pprDeclarator :: Maybe Id -> Decl -> Doc
pprDeclarator maybe_x declarator =
    case maybe_x of
      Nothing  -> snd $ pprDecl declarator empty
      Just x   -> snd $ pprDecl declarator (ppr x)
    where
      pprPtr :: Decl -> Doc -> (Decl, Doc)
      pprPtr (Ptr [] decl) post =
          pprPtr decl $
          text "*" <> post
      pprPtr (Ptr quals decl) post =
          pprPtr decl $
          text "*" <> spread (map ppr quals) <+> post
      pprPtr decl post = (decl, post)

      pprDirDecl :: Decl -> Doc -> (Decl, Doc)
      pprDirDecl (Array decl dim) pre =
          pprDirDecl decl $
          pre <> brackets (ppr dim)

      pprDirDecl (Proto decl args) pre =
          pprDirDecl decl $
          pre <> parens (ppr args)

      pprDirDecl (OldProto decl args) pre =
          pprDirDecl decl $
          pre <> parens (commasep (map ppr args))

      pprDirDecl decl pre = (decl, pre)

      pprDecl :: Decl -> Doc -> (Decl, Doc)
      pprDecl decl mid =
          case decl'' of
            DeclRoot  -> (decl'', post)
            _         -> pprDecl decl'' $
                         parens post
        where
          (decl', pre)   = pprDirDecl decl mid
          (decl'', post) = pprPtr decl' pre

instance Pretty Type where
    ppr (Type spec decl) = ppr spec <+/> pprDeclarator Nothing decl

    ppr (AntiType v) = ppr "$ty:" <> ppr v

instance Pretty Designator where
    ppr (IndexDesignator e)       = brackets $ ppr e
    ppr (MemberDesignator ident)  = dot <> ppr ident

instance Pretty Designation where
    ppr (Designation ds) = folddoc (<>) (map ppr ds)

instance Pretty Initializer where
    ppr (ExpInitializer e) = ppr e

    ppr (CompoundInitializer inits) =
        braces $ commasep (map pprInit inits)
      where
        pprInit :: (Maybe Designation, Initializer) -> Doc
        pprInit (Nothing, init) = ppr init
        pprInit (Just d, init)  = ppr d <+> text "=" <+/> ppr init

instance Pretty Init where
    ppr (Init ident decl maybe_e attrs) =
        pprDeclarator (Just ident) decl
        <+> case maybe_e of
              Nothing -> empty
              Just e  -> text "=" <+/> ppr e
        <+> ppr attrs

instance Pretty Typedef where
    ppr (Typedef ident decl attrs) = ppr (Init ident decl Nothing attrs)

instance Pretty InitGroup where
    ppr (InitGroup spec attrs inits) =
        (ppr spec <+> ppr attrs
         <+> commasep (map ppr inits))
        <> semi

    ppr (TypedefGroup spec attrs typedefs) =
        (text "typedef" <+> ppr spec <+> ppr attrs
         <+> commasep (map ppr typedefs))
        <> semi

    ppr (AntiDecls v)  = ppr "$decls:"  <> ppr v
    ppr (AntiDecl v)   = ppr "$decl:"   <> ppr v

instance Pretty Field where
    ppr (Field maybe_id maybe_decl maybe_e) =
        case maybe_decl of
          Nothing   -> empty
          Just decl -> pprDeclarator maybe_id decl
        <+>
        case maybe_e of
          Nothing -> empty
          Just e  -> colon <+> ppr e

instance Pretty FieldGroup where
    ppr (FieldGroup spec fields) =
        ppr spec <+> commasep (map ppr fields)

    ppr (AntiSdecls v)  = ppr "$sdecls:"  <> ppr v
    ppr (AntiSdecl v)   = ppr "$sdecl:"   <> ppr v

instance Pretty CEnum where
    ppr (CEnum ident maybe_e) =
        ppr ident
        <+> case maybe_e of
              Nothing  -> empty
              Just e -> text "=" <+/> ppr e

    ppr (AntiEnums v)  = ppr "$enums:"  <> ppr v
    ppr (AntiEnum v)   = ppr "$enum:"   <> ppr v

instance Pretty Attr where
    ppr (Attr ident []) = ppr ident
    ppr (Attr ident args) =
        ppr ident <> parens (commasep (map ppr args))

    pprList []    = empty
    pprList attrs = text "__attribute__" <>
                    parens (parens (commasep (map ppr attrs)))

instance Pretty Param where
    ppr (Param maybe_id spec decl) =
        ppr spec <+> pprDeclarator maybe_id decl

    ppr (AntiParams v)  = ppr "$params:"  <> ppr v
    ppr (AntiParam v)   = ppr "$param:"   <> ppr v

instance Pretty Params where
    ppr (Params args varargs) =
        commasep (map ppr args)
        <> (if varargs then comma <+> text "..." else empty)

instance Pretty Func where
    ppr (Func spec ident decl args stm) =
        ppr spec <+> pprDeclarator (Just ident) (Proto decl args)
        </> ppr stm

    ppr (OldFunc spec ident decl args maybe_initgroups stm) =
        ppr spec <+> pprDeclarator (Just ident) (OldProto decl args)
        </> case maybe_initgroups of
              Nothing         -> empty
              Just initgroups -> seplines semi (map ppr initgroups)
        </> ppr stm

instance Pretty Definition where
    ppr (FuncDef func sloc)     = srcloc sloc <> ppr func
    ppr (DecDef initgroup sloc) = srcloc sloc <> ppr initgroup
    ppr (EscDef s sloc) = srcloc sloc <> text s

    ppr (AntiEdecls v _)  = ppr "$edecls:"  <> ppr v
    ppr (AntiEdecl v _)   = ppr "$edecl:"   <> ppr v
    ppr (AntiFunc v _)    = ppr "$func:"    <> ppr v

    pprList = foldl (<>) empty . intersperse line . map ppr

instance Pretty Stm where
    ppr (Label ident stm sloc) =
        srcloc sloc <>
        nest (-2) (line <> ppr ident <+> colon </> ppr stm)
    ppr (Case e stm sloc) =
        srcloc sloc <>
        nest (-2) (line <> text "case" <+> ppr e <> colon) </> ppr stm
    ppr (Default stm sloc) =
        srcloc sloc <>
        nest (-2) (line <> text "default:") </> ppr stm
    ppr (Exp maybe_e sloc) =
        srcloc sloc <>
        ppr maybe_e <> semi
    ppr (Block [] [] sloc) =
        srcloc sloc <>
        text "{" </>
        text "}"
    ppr (Block initgroups [] sloc) =
        srcloc sloc <>
        nest 4 (text "{" </>
                stack (map ppr initgroups)) </>
        text "}"
    ppr (Block [] stms sloc) =
        srcloc sloc <>
        nest 4 (text "{" </>
                stack (map ppr stms)) </>
        text "}"
    ppr (Block initgroups stms sloc) =
        srcloc sloc <>
        nest 4 (text "{" </>
                stack (map ppr initgroups) </> line <>
                stack (map ppr stms)) </>
        text "}"
    ppr (If test then' maybe_else sloc) =
        srcloc sloc <>
        text "if" <> parens (ppr test)
        <> pprStm then'
        <> case maybe_else of
             Nothing     -> empty
             Just else'  -> space <> text "else" <> pprStm else'
      where
        pprStm :: Stm -> Doc
        pprStm stm@(Block _ _ _)   = space <> ppr stm
        pprStm stm@(If _ _ _ _)    = space <> ppr stm
        pprStm stm                 = nest 4 (line <> ppr stm) <> line
    ppr (Switch e stm sloc) =
        srcloc sloc <>
        text "switch" <> parens (ppr e ) <+/> ppr stm
    ppr (While e stm sloc) =
        srcloc sloc <>
        text "while" <> parens (ppr e) <+/> ppr stm
    ppr (DoWhile stm e sloc) =
        srcloc sloc <>
        text "do" <+/> ppr stm <+/> text "while" <> parens(ppr e) <> semi
    ppr (For ini test post stm sloc) =
        srcloc sloc <>
        text "for"
        <> parens (semisep [ppr ini, ppr test, ppr post])
        <+/> ppr stm
    ppr (Goto ident sloc) =
        srcloc sloc <>
        text "goto" <+> ppr ident <> semi
    ppr (Continue sloc)        = srcloc sloc <> text "continue" <>semi
    ppr (Break sloc)           = srcloc sloc <> text "break" <> semi
    ppr (Return Nothing sloc)  = srcloc sloc <> text "return" <> semi
    ppr (Return (Just e) sloc) = srcloc sloc <> text "return" <+> ppr e <> semi
    ppr (Asm _ template outputs inputs clobbered sloc) =
        srcloc sloc <>
        text "__asm__" <>
        parens((spread $ map (dquotes . text) template)
               <+> (if length outputs == 0
                    then empty
                    else colon <+> pregs outputs)
               <+> (if length inputs == 0
                    then empty
                    else colon <+> pregs inputs)
               <+> (if length clobbered == 0
                    then empty
                    else colon <+> commasep (map text clobbered)))
      where
        pregs :: [(String, Exp)] -> Doc
        pregs regs = commasep (map preg regs)

        preg :: (String, Exp) -> Doc
        preg (reg, e) = dquotes (text reg) <+> parens (ppr e)

    ppr (AntiStm v _)  = text $ "$stm:" ++ v ++ "$"
    ppr (AntiStms v _) = text $ "$stms:" ++ v ++ "$"

instance Pretty Const where
    ppr (IntConst (s, _, _))          = text s
    ppr (LongIntConst (s, _, _))      = text s
    ppr (LongLongIntConst (s, _, _))  = text s
    ppr (FloatConst (s, _))           = text s
    ppr (DoubleConst (s, _))          = text s
    ppr (LongDoubleConst (s, _))      = text s
    ppr (CharConst c) | isPrint c     = text $ show c
                      | ord c == 0    = squotes $ text $ "\\0"
                      | otherwise     = squotes $ text $
                                        "\\x" ++ showHex (ord c) ""
    ppr (StringConst s)               = text $ show s

    ppr (AntiString v)      = ppr "$string:"   <> ppr v
    ppr (AntiChar v)        = ppr "$char:"     <> ppr v
    ppr (AntiLongDouble v)  = ppr "$ldouble:"  <> ppr v
    ppr (AntiDouble v)      = ppr "$double:"   <> ppr v
    ppr (AntiFloat v)       = ppr "$float:"    <> ppr v
    ppr (AntiULInt v)       = ppr "$ulint:"    <> ppr v
    ppr (AntiLInt v)        = ppr "$lint:"     <> ppr v
    ppr (AntiUInt v)        = ppr "$uint:"     <> ppr v
    ppr (AntiInt v)         = ppr "$int:"      <> ppr v

instance Pretty Exp where
    pprPrec _ (Var ident sloc) = srcloc sloc <> ppr ident
    pprPrec _ (Const k sloc) = srcloc sloc <> ppr k
    pprPrec p (BinOp op e1 e2 sloc) =
        srcloc sloc <>
        infixOp p (fixity op) (ppr op) e1 e2
    pprPrec p (Assign op e1 e2 sloc) =
        srcloc sloc <>
        infixOp p (fixity op) (ppr op) e1 e2
    pprPrec p (PreInc e sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (text "++" <> pprPrec 14 e)
    pprPrec p (PostInc e sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (pprPrec 14 e <> text "++")
    pprPrec p (PreDec e sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (text "--" <> pprPrec 14 e)
    pprPrec p (PostDec e sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (pprPrec 14 e <> text "--")
    pprPrec p (UnOp op e sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (ppr op <> pprPrec 14 e)
    pprPrec p (SizeofExp e sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (text "sizeof" <> parens (pprPrec 14 e))
    pprPrec p (SizeofType tipe sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (text "sizeof" <> parens (ppr tipe))
    pprPrec p (Cast tipe e sloc) =
        srcloc sloc <>
        parensIf (p > 14)
                 (parens (ppr tipe) <+> pprPrec 14 e)
    pprPrec p (Cond test then' else' sloc) =
        srcloc sloc <>
        parensIf (p > 3)
                 (pprPrec 3 test <+> text "?" <+> pprPrec 3 then'
                                 <+> colon <+> pprPrec 3 else')
    pprPrec p (Member e ident sloc) =
        srcloc sloc <>
        parensIf (p > 15)
                 (pprPrec 15 e <> dot <> ppr ident)
    pprPrec p (PtrMember e ident sloc) =
        srcloc sloc <>
        parensIf (p > 15)
                 (pprPrec 15 e <> text "->" <> ppr ident)
    pprPrec p (Index e1 e2 sloc) =
        srcloc sloc <>
        parensIf (p > 15)
                 (pprPrec 15 e1 <> brackets (ppr e2))
    pprPrec p (FnCall f args sloc) =
        srcloc sloc <>
        parensIf (p > 15)
                 (pprPrec 15 f <> parens (commasep (map ppr args)))
    pprPrec p (Seq e1 e2 sloc) =
        srcloc sloc <>
        parensIf (p > 1)
                 (pprPrec 1 e1 <> comma <+/> pprPrec 1 e2)
    pprPrec _ (StmExpr initgroups stms sloc) =
        srcloc sloc <>
        parens (ppr (Block initgroups stms sloc))
    pprPrec _ (BuiltinVaArg e ty sloc) =
        srcloc sloc <>
        text "__builtin_va_arg(" <> ppr e <> comma <+> ppr ty <> rparen

    pprPrec _ (AntiArgs v _)  = text "$args:" <> text v
    pprPrec _ (AntiExp v _)   = text "$var:" <> text v

instance Pretty BinOp where
    ppr Add  = text "+"
    ppr Sub  = text "-"
    ppr Mul  = text "*"
    ppr Div  = text "/"
    ppr Mod  = text "%"
    ppr Eq   = text "=="
    ppr Ne   = text "!="
    ppr Lt   = text "<"
    ppr Gt   = text ">"
    ppr Le   = text "<="
    ppr Ge   = text ">="
    ppr Land = text "&&"
    ppr Lor  = text "||"
    ppr And  = text "&"
    ppr Or   = text "|"
    ppr Xor  = text "^"
    ppr Lsh  = text "<<"
    ppr Rsh  = text ">>"

instance Pretty AssignOp where
    ppr JustAssign = text "="
    ppr AddAssign  = text "+="
    ppr SubAssign  = text "-="
    ppr MulAssign  = text "*="
    ppr DivAssign  = text "/="
    ppr ModAssign  = text "%="
    ppr LshAssign  = text "<<="
    ppr RshAssign  = text ">>="
    ppr AndAssign  = text "&="
    ppr XorAssign  = text "^="
    ppr OrAssign   = text "|="

instance Pretty UnOp where
    ppr AddrOf   = text "&"
    ppr Deref    = text "*"
    ppr Positive = text "+"
    ppr Negate   = text "-"
    ppr Not      = text "~"
    ppr Lnot     = text "!"
