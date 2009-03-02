\begin{code}
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
-- Module      :  Language.Hs.Quote
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Quote
    (quasiOpts,
     topdecls,
     decls,
     ty,
     exp)
  where

import Prelude hiding (exp, init)

import qualified Data.ByteString.Char8 as B hiding (init, inits)
import Data.Generics
import Data.IORef
import Foreign (unsafePerformIO)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Compiler.Opt
import Data.IString
import Data.Loc
import qualified Data.Name as N
import qualified Language.Hs as H
import qualified Language.Hs.Parser as P

stringExp :: String -> Maybe (Q Exp)
stringExp s = Just $ litE $ stringL s

istringExp :: IString -> Maybe (Q Exp)
istringExp is = Just [|istring $(litE $ stringL (istringString is))|]

antiLitExp :: H.Lit -> Maybe (Q Exp)
antiLitExp (H.AntiInt v)   = Just $ [|H.IntegerLit $(varE $ mkName v)|]
antiLitExp (H.AntiFloat v) = Just $ [|H.FloatLit $(varE $ mkName v)|]
antiLitExp _               = Nothing

antiVarExp :: H.Var -> Maybe (Q Exp)
antiVarExp (H.AntiVar v)   = Just $ varE $ mkName v
antiVarExp (H.AntiVarId v) = Just $ [|H.Var $ N.mkName $(varE $ mkName v)|]
antiVarExp _               = Nothing

antiTypeExp :: H.Type -> Maybe (Q Exp)
antiTypeExp (H.AntiType v) = Just $ varE $ mkName v
antiTypeExp _              = Nothing

antiExpExp :: H.Exp -> Maybe (Q Exp)
antiExpExp (H.AntiExp v _) = Just $ varE $ mkName v
antiExpExp _               = Nothing

antiExp :: Typeable a => a -> Maybe (Q Exp)
antiExp = const Nothing  `extQ`  stringExp
                         `extQ`  istringExp
                         `extQ`  antiLitExp
                         `extQ`  antiVarExp
                         `extQ`  antiTypeExp
                         `extQ`  antiExpExp

stringPat :: String -> Maybe (Q Pat)
stringPat s = Just $ litP $ stringL s

istringPat :: IString -> Maybe (Q Pat)
istringPat is =
    Just $ conP (mkName "IString") [litP $ stringL (istringString is), wildP]

locPat :: Data.Loc.Loc -> Maybe (Q Pat)
locPat _ = Just $ wildP

antiPat :: Typeable a => a -> Maybe (Q Pat)
antiPat = const Nothing `extQ` stringPat `extQ` istringPat `extQ` locPat

quasiOpts :: IORef Opts
quasiOpts = unsafePerformIO $ newIORef $
            Opts { output = Nothing, flags = [] }

parse  ::  Monad m
       =>  String
       ->  (String, Int, Int)
       ->  Bool
       ->  P.ParseContext
       ->  P.P a
       ->  m a
parse s (file, line, col) isModule ctxt p =
    do  let pos = Pos file col line col
        let opts = unsafePerformIO $ readIORef quasiOpts
        case P.parse (B.pack s) pos isModule opts ctxt p of
          Left err -> unsafePerformIO $ fail $ show err
          Right x  -> return x

parse1 :: P.P (L a)
       -> Bool
       -> P.ParseContext
       -> (a -> Q b)
       -> (String -> Q b)
parse1 p isModule ctxt dataToQ s
    = do  loc <- location
          let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
          L _ x <- parse s pos isModule ctxt p
          dataToQ x

parseList :: P.P [L a]
          -> Bool
          -> P.ParseContext
          -> ([a] -> Q b)
          -> (String -> Q b)
parseList p isModule ctxt dataToQ s
    = do  loc <- location
          let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
          xs <-  parse s pos isModule ctxt p
          dataToQ $ map unLoc xs

qausiquote1 :: Data a
            => P.P (L a)
            -> Bool
            -> QuasiQuoter
qausiquote1 p isModule
    = QuasiQuoter  (parse1 p isModule P.ParseExpression (dataToExpQ antiExp))
                   (parse1 p isModule  P.ParsePattern (dataToPatQ antiPat))

qausiquoteList :: Data a
               => P.P [L a]
               -> Bool
               -> QuasiQuoter
qausiquoteList p isModule
    = QuasiQuoter  (parseList p isModule P.ParseExpression (dataToExpQ antiExp))
                   (parseList p isModule P.ParsePattern (dataToPatQ antiPat))

topdecls :: QuasiQuoter
topdecls  = qausiquoteList P.parseTopDecls False

decls :: QuasiQuoter
decls  = qausiquoteList P.parseDecls True

ty :: QuasiQuoter
ty   = qausiquote1 P.parseType False

exp :: QuasiQuoter
exp   = qausiquote1 P.parseExp False
\end{code}

\begin{code}
unqualifiedConstructors :: [String]
unqualifiedConstructors = ["[]", "(,)", "(,,)",
                           "True", "False",
                           "Just", "Nothing",
                           "Left", "Right",
                           "Loc", "Pos", "SrcLoc",
                           "Name", "Unqual", "IString",
                           "LeftAssoc", "RightAssoc", "NonAssoc"]

dataToQa  ::  forall a k q. Data a
          =>  (Name -> k)
          ->  (Lit -> Q q)
          ->  (k -> [Q q] -> Q q)
          ->  (forall a . Data a => a -> Maybe (Q q))
          ->  a
          ->  Q q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Nothing ->
          case constrRep constr of
            AlgConstr _  ->
                appCon con conArgs
            IntConstr n ->
                mkLit $ integerL n
            FloatConstr n ->
                mkLit $ rationalL (toRational n)
            StringConstr [c] ->
                mkLit $ charL c
            StringConstr s ->
                mkLit $ stringL s
        where
          constr :: Constr
          constr = toConstr t
          constrName :: Constr -> String
          constrName k =
              case showConstr k of
                "(:)"  -> ":"
                name   -> if name `elem` unqualifiedConstructors
                          then name
                          else "Language.Hs.Syntax." ++ name
          con :: k
          con = mkCon (mkName (constrName constr))
          conArgs :: [Q q]
          conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t

      Just y -> y

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
\end{code}
