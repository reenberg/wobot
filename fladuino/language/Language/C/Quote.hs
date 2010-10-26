{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
-- Module      :  Language.C.Quote
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.C.Quote
    (cexp,
     cedecl,
     cdecl,
     csdecl,
     cenum,
     cty,
     cparam,
     cinit,
     cstm,
     cunit,
     cfun)
    where

import Prelude hiding (exp, init)

import qualified Data.ByteString.Char8 as B hiding (init, inits)
import Data.Generics
import Foreign (unsafePerformIO)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Loc
import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C

stringExp :: String -> Maybe (Q Exp)
stringExp s = Just $ litE $ stringL s

antiIdExp :: C.Id -> Maybe (Q Exp)
antiIdExp (C.AntiId v) = Just $ [|C.Id $(varE $ mkName v)|]
antiIdExp _            = Nothing

antiDeclSpecExp :: C.DeclSpec -> Maybe (Q Exp)
antiDeclSpecExp (C.AntiTypeDeclSpec extraStorage extraTypeQuals v) =
    Just [|let C.Type (C.DeclSpec storage typeQuals typeSpec) _
                   = $(varE $ mkName v)
           in
             C.DeclSpec (storage ++ $(dataToExpQ antiExp extraStorage))
                        (typeQuals ++ $(dataToExpQ antiExp extraTypeQuals))
                        typeSpec
         |]
antiDeclSpecExp _ = Nothing

antiDeclExp :: C.Decl -> Maybe (Q Exp)
antiDeclExp (C.AntiTypeDecl  v) =
    Just [|let C.Type _ decl = $(varE $ mkName v) in decl|]
antiDeclExp _ = Nothing

antiTypeExp :: C.Type -> Maybe (Q Exp)
antiTypeExp (C.AntiType v) = Just $ varE $ mkName v
antiTypeExp _              = Nothing

antiInitGroupExp :: C.InitGroup -> Maybe (Q Exp)
antiInitGroupExp (C.AntiDecl v) = Just $ varE $ mkName v
antiInitGroupExp _              = Nothing

antiInitGroupListExp :: [C.InitGroup] -> Maybe (Q Exp)
antiInitGroupListExp [] = Just [|[]|]
antiInitGroupListExp (C.AntiDecls v : inits) =
    Just [|$(varE $ mkName v) ++ $(dataToExpQ antiExp inits)|]
antiInitGroupListExp (init : inits) =
    Just [|$(dataToExpQ antiExp init) : $(dataToExpQ antiExp inits)|]

antiFieldGroupExp :: C.FieldGroup -> Maybe (Q Exp)
antiFieldGroupExp (C.AntiSdecl v) = Just $ varE $ mkName v
antiFieldGroupExp _               = Nothing

antiFieldGroupListExp :: [C.FieldGroup] -> Maybe (Q Exp)
antiFieldGroupListExp [] = Just [|[]|]
antiFieldGroupListExp (C.AntiSdecls v : fields) =
    Just [|$(varE $ mkName v) ++ $(dataToExpQ antiExp fields)|]
antiFieldGroupListExp (field : fields) =
    Just [|$(dataToExpQ antiExp field) : $(dataToExpQ antiExp fields)|]

antiCEnumExp :: C.CEnum -> Maybe (Q Exp)
antiCEnumExp (C.AntiEnum v) = Just $ varE $ mkName v
antiCEnumExp _              = Nothing

antiCEnumListExp :: [C.CEnum] -> Maybe (Q Exp)
antiCEnumListExp [] = Just [|[]|]
antiCEnumListExp (C.AntiEnums v : fields) =
    Just [|$(varE $ mkName v) ++ $(dataToExpQ antiExp fields)|]
antiCEnumListExp (field : fields) =
    Just [|$(dataToExpQ antiExp field) : $(dataToExpQ antiExp fields)|]

antiParamExp :: C.Param -> Maybe (Q Exp)
antiParamExp (C.AntiParam v) = Just $ varE $ mkName v
antiParamExp _               = Nothing

antiParamListExp :: [C.Param] -> Maybe (Q Exp)
antiParamListExp [] = Just [|[]|]
antiParamListExp (C.AntiParams v : args) =
    Just [|$(varE $ mkName v) ++ $(dataToExpQ antiExp args)|]
antiParamListExp (arg : args) =
    Just [|$(dataToExpQ antiExp arg) : $(dataToExpQ antiExp args)|]

antiDefinitionExp :: C.Definition -> Maybe (Q Exp)
antiDefinitionExp (C.AntiFunc v sloc) =
    Just $ [|C.FuncDef $(varE $ mkName v) $(dataToExpQ antiExp sloc)|]
antiDefinitionExp (C.AntiEdecl v _) =
    Just $ varE $ mkName v
antiDefinitionExp _ = Nothing

antiDefinitionListExp :: [C.Definition] -> Maybe (Q Exp)
antiDefinitionListExp [] = Just [|[]|]
antiDefinitionListExp (C.AntiEdecls v sloc : defs) =
    Just [|$(varE $ mkName v) ++ $(dataToExpQ antiExp defs)|]
antiDefinitionListExp (def : defs) =
    Just [|$(dataToExpQ antiExp def) : $(dataToExpQ antiExp defs)|]

antiConstExp :: C.Const -> Maybe (Q Exp)
antiConstExp (C.AntiInt v) =
    Just [|C.IntConst (show $(varE $ mkName v),
                       False, $(varE $ mkName v))|]
antiConstExp (C.AntiUInt v) =
    Just [|C.IntConst (show $(varE $ mkName v) ++ "U",
                       True, $(varE $ mkName v))|]
antiConstExp (C.AntiLInt v) =
    Just [|C.LongIntConst (show $(varE $ mkName v) ++ "L",
                           False, $(varE $ mkName v))|]
antiConstExp (C.AntiULInt v) =
    Just [|C.LongIntConst (show $(varE $ mkName v) ++ "UL",
                           True, $(varE $ mkName v))|]
antiConstExp (C.AntiFloat v) =
    Just [|C.FloatConst (show $(varE $ mkName v) ++ "F",
                         $(varE $ mkName v))|]
antiConstExp (C.AntiDouble v) =
    Just [|C.DoubleConst (show $(varE $ mkName v),
                          $(varE $ mkName v))|]
antiConstExp (C.AntiLongDouble v) =
    Just [|C.LongDoubleConst (show $(varE $ mkName v) ++ "L",
                              $(varE $ mkName v))|]
antiConstExp (C.AntiChar v) =
    Just [|C.CharConst $(varE $ mkName v)|]
antiConstExp (C.AntiString v) =
    Just [|C.StringConst $(varE $ mkName v)|]
antiConstExp _ = Nothing

antiExpExp :: C.Exp -> Maybe (Q Exp)
antiExpExp (C.AntiExp v _) = Just $ varE $ mkName v
antiExpExp _               = Nothing

antiExpListExp :: [C.Exp] -> Maybe (Q Exp)
antiExpListExp [] = Just [|[]|]
antiExpListExp (C.AntiArgs v _ : exps) =
    Just [|$(varE $ mkName v) ++ $(dataToExpQ antiExp exps)|]
antiExpListExp (exp : exps) =
    Just [|$(dataToExpQ antiExp exp) : $(dataToExpQ antiExp exps)|]

antiStmExp :: C.Stm -> Maybe (Q Exp)
antiStmExp (C.AntiStm v _) = Just $ varE $ mkName v
antiStmExp _               = Nothing

antiStmListExp :: [C.Stm] -> Maybe (Q Exp)
antiStmListExp [] = Just [|[]|]
antiStmListExp (C.AntiStms v _ : stms) =
    Just [|$(varE $ mkName v) ++ $(dataToExpQ antiExp stms)|]
antiStmListExp (stm : stms) =
    Just [|$(dataToExpQ antiExp stm) : $(dataToExpQ antiExp stms)|]

antiExp :: Typeable a => a -> Maybe (Q Exp)
antiExp = const Nothing  `extQ` stringExp
                         `extQ` antiIdExp
                         `extQ` antiDeclSpecExp
                         `extQ` antiDeclExp
                         `extQ` antiTypeExp
                         `extQ` antiInitGroupExp
                         `extQ` antiInitGroupListExp
                         `extQ` antiFieldGroupExp
                         `extQ` antiFieldGroupListExp
                         `extQ` antiCEnumExp
                         `extQ` antiCEnumListExp
                         `extQ` antiParamExp
                         `extQ` antiParamListExp
                         `extQ` antiDefinitionExp
                         `extQ` antiDefinitionListExp
                         `extQ` antiConstExp
                         `extQ` antiExpExp
                         `extQ` antiExpListExp
                         `extQ` antiStmExp
                         `extQ` antiStmListExp

stringPat :: String -> Maybe (Q Pat)
stringPat s = Just $ litP $ stringL s

locPat :: Data.Loc.Loc -> Maybe (Q Pat)
locPat _ = Just $ wildP

antiPat :: Typeable a => a -> Maybe (Q Pat)
antiPat = const Nothing `extQ` stringPat
                        `extQ` locPat

parse :: Monad m
      => (String, Int, Int)
      -> P.ParseContext
      -> Bool
      -> Bool
      -> P.P a
      -> String
      -> m a
parse (file, line, col) ctx c99 gcc p s =
    case P.parse ctx c99 gcc p start buf of
      Left err -> unsafePerformIO $ fail $ show err
      Right x  -> return x
  where
    start :: Pos
    start = Pos file col line col

    buf :: B.ByteString
    buf = B.pack s

parse1 :: P.P (L a)
       -> P.ParseContext
       -> (a -> Q b)
       -> (String -> Q b)
parse1 p ctx dataToQ s
    = do  loc <- location
          let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
          L _ x <- parse pos ctx True True p s
          dataToQ x

parseList :: P.P [L a]
          -> P.ParseContext
          -> ([a] -> Q b)
          -> (String -> Q b)
parseList p ctx dataToQ s
    = do  loc <- location
          let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
          xs <- parse pos ctx True True p s
          dataToQ $ map unLoc xs

qausiquote1 :: Data a
            => P.P (L a)
            -> QuasiQuoter
qausiquote1 p
    = QuasiQuoter  (parse1 p P.ParseExpression (dataToExpQ antiExp))
                   (parse1 p P.ParsePattern (dataToPatQ antiPat))

qausiquoteList :: Data a
               => P.P [L a]
               -> QuasiQuoter
qausiquoteList p
    = QuasiQuoter  (parseList p P.ParseExpression (dataToExpQ antiExp))
                   (parseList p P.ParsePattern (dataToPatQ antiPat))

cexp :: QuasiQuoter
cexp   = qausiquote1 P.parseExp

cedecl :: QuasiQuoter
cedecl  = qausiquote1 P.parseEdecl

cdecl :: QuasiQuoter
cdecl  = qausiquote1 P.parseDecl

csdecl :: QuasiQuoter
csdecl = qausiquote1 P.parseStructDecl

cenum :: QuasiQuoter
cenum  = qausiquote1 P.parseEnum

cty :: QuasiQuoter
cty    = qausiquote1 P.parseType

cparam :: QuasiQuoter
cparam = qausiquote1 P.parseParam

cinit :: QuasiQuoter
cinit  = qausiquote1 P.parseInit

cstm :: QuasiQuoter
cstm   = qausiquote1 P.parseStm

cunit :: QuasiQuoter
cunit = qausiquoteList P.parseUnit

cfun :: QuasiQuoter
cfun = qausiquote1 P.parseFunc

unqualifiedConstructors :: [String]
unqualifiedConstructors = ["[]", "(,)", "(,,)",
                           "True", "False",
                           "Just", "Nothing",
                           "Left", "Right",
                           "Loc", "Pos", "SrcLoc"]

dataToQa  ::  forall a k q. Data a
          =>  (Name -> k)
          ->  (Lit -> Q q)
          ->  (k -> [Q q] -> Q q)
          ->  (forall a . Data a => a -> Maybe (Q q))
          ->  a
          ->  Q q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Just x  -> x
      Nothing ->
          case constrRep constr of
            AlgConstr _      -> appCon con conArgs
            IntConstr n      -> mkLit $ integerL n
            FloatConstr n    -> mkLit $ rationalL (toRational n)
            CharConstr c     -> mkLit $ charL c
        where
          constr :: Constr
          constr = toConstr t
          constrName :: Constr -> String
          constrName k =
              case showConstr k of
                "(:)"  -> ":"
                name   -> if name `elem` unqualifiedConstructors
                          then name
                          else "Language.C.Syntax." ++ name
          con :: k
          con = mkCon (mkName (constrName constr))
          conArgs :: [Q q]
          conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t

dataToExpQ  ::  Data a
            =>  (forall a . Data a => a -> Maybe (Q Exp))
            ->  a
            ->  Q Exp
dataToExpQ = dataToQa conE litE (foldl appE)

dataToPatQ  ::  Data a
            =>  (forall a . Data a => a -> Maybe (Q Pat))
            ->  a
            ->  Q Pat
dataToPatQ = dataToQa id litP conP
