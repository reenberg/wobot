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
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.C.Syntax where

import Prelude hiding (init)

import Data.Generics

import Data.Loc

data Id = Id String
        | AntiId String
    deriving (Eq, Ord, Data, Typeable)

data Storage = Tauto
             | Tregister
             | Tstatic
             | Textern
             | Ttypedef
    deriving (Eq, Ord, Data, Typeable)

data TypeQual = Tconst
              | Tvolatile
              | Tinline
    deriving (Eq, Ord, Data, Typeable)

data Sign = Tsigned
          | Tunsigned
    deriving (Eq, Ord, Data, Typeable)

data TypeSpec = Tvoid
              | Tchar (Maybe Sign)
              | Tshort (Maybe Sign)
              | Tint (Maybe Sign)
              | Tlong (Maybe Sign)
              | Tlong_long (Maybe Sign)
              | Tfloat
              | Tdouble
              | Tlong_double
              | Tstruct (Maybe Id) (Maybe [FieldGroup]) [Attr]
              | Tunion (Maybe Id) (Maybe [FieldGroup]) [Attr]
              | Tenum (Maybe Id) [CEnum] [Attr]
              | Tnamed Id
              | Tva_list
    deriving (Eq, Ord, Data, Typeable)

data DeclSpec = DeclSpec [Storage] [TypeQual] TypeSpec
              | AntiTypeDeclSpec [Storage] [TypeQual] String
    deriving (Eq, Ord, Data, Typeable)

-- | There are two types of declarators in C, regular declarators and abstract
-- declarators. The former is for declaring variables, function parameters,
-- typedefs, etc. and the latter for abstract types---@typedef int
-- ({*}foo)(void)@ vs. @\tt int ({*})(void)@. The difference between the two is
-- just whether or not an identifier is attached to the declarator. We therefore
-- only define one 'Decl' type and use it for both cases.

data Decl = DeclRoot
          | Ptr [TypeQual] Decl
          | Array Decl (Maybe Exp)
          | Proto Decl Params
          | OldProto Decl [Id]
          | AntiTypeDecl String
    deriving (Eq, Ord, Data, Typeable)

data Type = Type DeclSpec Decl
          | AntiType String
    deriving (Eq, Ord, Data, Typeable)

data Designator = IndexDesignator Exp
                | MemberDesignator Id
    deriving (Eq, Ord, Data, Typeable)

data Designation = Designation [Designator]
    deriving (Eq, Ord, Data, Typeable)

data Initializer = ExpInitializer Exp
                 | CompoundInitializer [(Maybe Designation, Initializer)]
    deriving (Eq, Ord, Data, Typeable)

data Init = Init Id Decl (Maybe Initializer) [Attr]
    deriving (Eq, Ord, Data, Typeable)

data Typedef = Typedef Id Decl [Attr]
    deriving (Eq, Ord, Data, Typeable)

data InitGroup = InitGroup DeclSpec [Attr] [Init]
               | TypedefGroup DeclSpec [Attr] [Typedef]
               | AntiDecl String
               | AntiDecls String
    deriving (Eq, Ord, Data, Typeable)

data Field = Field (Maybe Id) (Maybe Decl) (Maybe Exp)
    deriving (Eq, Ord, Data, Typeable)

data FieldGroup  =  FieldGroup DeclSpec [Field]
                 |  AntiSdecl String
                 |  AntiSdecls String
    deriving (Eq, Ord, Data, Typeable)

data CEnum  =  CEnum Id (Maybe Exp)
            |  AntiEnum String
            |  AntiEnums String
    deriving (Eq, Ord, Data, Typeable)

data Attr  =  Attr Id [Exp]
    deriving (Eq, Ord, Data, Typeable)

data Param  =  Param (Maybe Id) DeclSpec Decl
            |  AntiParam String
            |  AntiParams String
    deriving (Eq, Ord, Data, Typeable)

data Params = Params [Param] Bool
    deriving (Eq, Ord, Data, Typeable)

data Func  =  Func DeclSpec Id Decl Params Stm
           |  OldFunc DeclSpec Id Decl [Id] (Maybe [InitGroup]) Stm
    deriving (Eq, Ord, Data, Typeable)

data Definition  =  FuncDef Func SrcLoc
                 |  DecDef InitGroup SrcLoc
                 |  EscDef String SrcLoc
                 |  AntiFunc String SrcLoc
                 |  AntiEdecl String SrcLoc
                 |  AntiEdecls String SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Stm  = Label Id Stm SrcLoc
          | Case Exp Stm SrcLoc
          | Default Stm SrcLoc
          | Exp (Maybe Exp) SrcLoc
          | Block [InitGroup] [Stm] SrcLoc
          | If Exp Stm (Maybe Stm) SrcLoc
          | Switch Exp Stm SrcLoc
          | While Exp Stm SrcLoc
          | DoWhile Stm Exp SrcLoc
          | For (Maybe Exp) (Maybe Exp) (Maybe Exp) Stm SrcLoc
          | Goto Id SrcLoc
          | Continue SrcLoc
          | Break SrcLoc
          | Return (Maybe Exp) SrcLoc
          | Asm  [Attr]
                 [String]         -- ^ template
                 [(String, Exp)]  -- ^ outputs
                 [(String, Exp)]  -- ^ inputs
                 [String]         -- ^ clobbered
                 SrcLoc
          | AntiStm String SrcLoc
          | AntiStms String SrcLoc
    deriving (Eq, Ord, Data, Typeable)

funcProto :: Func -> InitGroup
funcProto (Func decl_spec id decl params _) =
    InitGroup decl_spec [] [Init id (Proto decl params) Nothing []]
funcProto (OldFunc decl_spec id decl params _ _) =
    InitGroup decl_spec [] [Init id (OldProto decl params) Nothing []]

isPtr :: Type -> Bool
isPtr  (Type _ decl)  = go decl
  where
    go  DeclRoot          = False
    go  (Ptr _ _)         = True
    go  (Array _ _)       = True
    go  (Proto _ _)       = False
    go  (OldProto _ _)    = False
    go  (AntiTypeDecl _)  = error "isPtr: encounter antiquoted type declaration"
isPtr  (AntiType _)   = error "isPtr: encounter antiquoted type"

data Const = IntConst (String, Bool, Integer)
           | LongIntConst (String, Bool, Integer)
           | LongLongIntConst (String, Bool, Integer)
           | FloatConst (String, Double)
           | DoubleConst (String, Double)
           | LongDoubleConst (String, Double)
           | CharConst Char
           | StringConst String
           | AntiInt String
           | AntiUInt String
           | AntiLInt String
           | AntiULInt String
           | AntiFloat String
           | AntiDouble String
           | AntiLongDouble String
           | AntiChar String
           | AntiString String
    deriving (Eq, Ord, Data, Typeable)

data Exp = Var Id SrcLoc
         | Const Const SrcLoc
         | BinOp BinOp Exp Exp SrcLoc
         | Assign AssignOp Exp Exp SrcLoc
         | PreInc Exp SrcLoc
         | PostInc Exp SrcLoc
         | PreDec Exp SrcLoc
         | PostDec Exp SrcLoc
         | UnOp UnOp Exp SrcLoc
         | SizeofExp Exp SrcLoc
         | SizeofType Type SrcLoc
         | Cast Type Exp SrcLoc
         | Cond Exp Exp Exp SrcLoc
         | Member Exp Id SrcLoc
         | PtrMember Exp Id SrcLoc
         | Index Exp Exp SrcLoc
         | FnCall Exp [Exp] SrcLoc
         | Seq Exp Exp SrcLoc
         | StmExpr [InitGroup] [Stm] SrcLoc
         | BuiltinVaArg Exp Type SrcLoc
         | AntiExp String SrcLoc
         | AntiArgs String SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Mod
           | Eq
           | Ne
           | Lt
           | Gt
           | Le
           | Ge
           | Land
           | Lor
           | And
           | Or
           | Xor
           | Lsh
           | Rsh
    deriving (Eq, Ord, Data, Typeable)

data AssignOp = JustAssign
              | AddAssign
              | SubAssign
              | MulAssign
              | DivAssign
              | ModAssign
              | LshAssign
              | RshAssign
              | AndAssign
              | XorAssign
              | OrAssign
    deriving (Eq, Ord, Data, Typeable)

data UnOp = AddrOf
          | Deref
          | Positive
          | Negate
          | Not
          | Lnot
    deriving (Eq, Ord, Data, Typeable)
