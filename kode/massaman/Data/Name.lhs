\chapter{Names and Bindings}

%if False
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
-- Module      :  Data.Name
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Data.Name where

import Char (isLower,
             isUpper)
import Data.Generics (Data,
                      Typeable)
import Data.List (foldl',
                  intersperse)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set ((\\),
                           union)

import Data.IString
import Data.Loc
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{Symbols}

\begin{code}
class (Ord n, Data n, Pretty n) => Symbol n where
    isVarid   :: n -> Bool
    isConid   :: n -> Bool
    isVarsym  :: n -> Bool
    isConsym  :: n -> Bool
\end{code}

\begin{code}
instance Symbol String where
    isVarid []       = error "internal error: zero-length varid"
    isVarid (c : _)  = isLower c

    isConid []       = error "internal error: zero-length conid"
    isConid (c : _)  = isUpper c

    isVarsym ('!' : _)  = True
    isVarsym ('#' : _)  = True
    isVarsym ('$' : _)  = True
    isVarsym ('%' : _)  = True
    isVarsym ('&' : _)  = True
    isVarsym ('*' : _)  = True
    isVarsym ('+' : _)  = True
    isVarsym ('.' : _)  = True
    isVarsym ('/' : _)  = True
    isVarsym ('<' : _)  = True
    isVarsym ('=' : _)  = True
    isVarsym ('>' : _)  = True
    isVarsym ('?' : _)  = True
    isVarsym ('@' : _)  = True
    isVarsym ('\\' : _) = True
    isVarsym ('^' : _)  = True
    isVarsym ('|' : _)  = True
    isVarsym ('-' : _)  = True
    isVarsym ('~' : _)  = True
    isVarsym _          = False

    isConsym (':' : _)  = True
    isConsym _          = False
\end{code}

\section{Names}

\begin{code}
newtype ModuleName = ModuleName IString
  deriving (Eq, Ord, Data, Typeable)

instance Pretty ModuleName where
    ppr (ModuleName s) = ppr s

instance Show ModuleName where
    show (ModuleName s) = show s
\end{code}

\begin{code}
data NameSort  =  Builtin
               |  External ModuleName
               |  Internal
               |  Unqual
               |  Qual ModuleName
  deriving (Eq, Ord, Data, Typeable, Show)

data Name = Name { n_sort    :: NameSort,
                   n_loc     :: Loc,
                   n_name    :: !IString
                 }
  deriving (Data, Typeable)

instance Located Name where
    getLoc = nameLoc

nameSort :: Name -> NameSort
nameSort = n_sort

nameLoc :: Name -> Loc
nameLoc = n_loc

nameName :: Name -> IString
nameName = n_name

nameString :: Name -> String
nameString = istringString . n_name

nameAlts :: Name -> [Name]
nameAlts (Name sort loc is) =
    [Name sort loc (istring $ istringString is ++ show i)
         | i <- [1 :: Integer ..]]

nameToName :: (String -> String) -> Name -> Name
nameToName f (Name s l n) = Name s l (istring $ f $ istringString n)

instance Eq Name where
    Name s1 _ n1 == Name s2 _ n2 = (s1, n1) == (s2, n2)

instance Ord Name where
    compare (Name s1 _ n1) (Name s2 _ n2) = compare (s1, n1) (s2, n2)

instance Pretty Name where
    ppr (Name { n_sort = Builtin, n_name = n }) =
        ppr n

    ppr (Name { n_sort = External mod, n_name = n }) =
        ppr mod <> text "." <> ppr n

    ppr (Name { n_sort = Internal, n_name = n }) =
        ppr n

    ppr (Name { n_sort = Unqual, n_name = n }) =
        ppr n

    ppr (Name { n_sort = Qual mod, n_name = n }) =
        ppr mod <> text "." <> ppr n

instance Show Name where
    show = pprint
\end{code}

\begin{code}
class NameBinding b where
    bindingName :: b -> Name
    bindingLoc  :: b -> Loc
\end{code}

\begin{code}
instance Symbol Name where
    isVarid   = isVarid   . istringString . n_name
    isConid   = isConid   . istringString . n_name
    isVarsym  = isVarsym  . istringString . n_name
    isConsym  = isConsym  . istringString . n_name
\end{code}

\section{The Name Monad}

\begin{code}
modPrelude = ModuleName (istring "Prelude")

modNames = [modPrelude]

builtin s = Name Unqual builtinLoc (istring s)

builtinNil     = builtin "[]"
builtinCons    = builtin ":"
builtinUnit    = builtin "()"
builtinArrow   = builtin "->"

builtinNames = [  builtinNil,
                  builtinCons,
                  builtinUnit,
                  builtinArrow
               ]

primName s  = Name Unqual builtinLoc (istring s)

primEqInt = builtin "eqInt#"

primNames = [  primEqInt]

prelName s = Name Unqual builtinLoc (istring s)

prelBool      = prelName "Bool"
prelTrue      = prelName "True"
prelFalse     = prelName "False"
prelMaybe     = prelName "Maybe"
prelNothing   = prelName "Nothing"
prelJust      = prelName "Just"
prelEither    = prelName "Either"
prelLeft      = prelName "Left"
prelRight     = prelName "Right"
prelOrdering  = prelName "Ordering"
prelLT        = prelName "LT"
prelEQ        = prelName "EQ"
prelGT        = prelName "GT"
prelChar      = prelName "Char"
prelString    = prelName "String"
prelInt       = prelName "Int"
prelInteger   = prelName "Integer"
prelFloat     = prelName "Float"
prelDouble    = prelName "Double"
prelRational  = prelName "Rational"
prelIO        = prelName "IO"

prelError      = prelName "error"
prelUndefined  = prelName "undefined"
prelPlus       = prelName "+"
prelNegate     = prelName "negate"

prelNames = [  prelBool,
               prelTrue,
               prelFalse,
               prelMaybe,
               prelNothing,
               prelJust,
               prelEither,
               prelLeft,
               prelRight,
               prelOrdering,
               prelLT,
               prelEQ,
               prelGT,
               prelChar,
               prelString,
               prelInt,
               prelInteger,
               prelFloat,
               prelDouble,
               prelRational,
               prelIO,

               prelError,
               prelUndefined,
               prelPlus,
               prelNegate
            ]
\end{code}

\begin{code}
mkName :: String -> Name
mkName s = mkUnqualName s internalLoc

mkNameAt :: String -> Loc -> Name
mkNameAt = mkUnqualName
\end{code}

\begin{code}
mkBuiltinName :: String -> Name
mkBuiltinName n = Name  {  n_sort  = Builtin,
                           n_loc   = builtinLoc,
                           n_name  = istring n
                        }
\end{code}

\begin{code}
mkInternalName :: String -> Name
mkInternalName n = Name  {  n_sort  = Internal,
                            n_loc   = internalLoc,
                            n_name  = istring n
                         }
\end{code}

\begin{code}
mkUnqualName :: String -> Loc -> Name
mkUnqualName n loc =  Name  {  n_sort  = Unqual,
                               n_loc   = loc,
                               n_name  = istring n
                            }
\end{code}

\begin{code}
mkQualName :: [String] -> String -> Loc -> Name
mkQualName mod n loc =
    Name  {  n_sort  = Qual modname,
             n_loc   = loc,
             n_name  = nname
          }
  where
    modname  = ModuleName $ istring $ concat $ intersperse "." mod
    nname    = istring n
\end{code}

\section{Calculating Free and Bound Variables}

We want to be able to calculate the free and bound variables of expressions, but
also the free and bound type variables and type constructors of types and type
declarations. Therefore we define multi-parameter type classes |FreeVars| ad
|Bound| where the first parameter is the type over which we are calculating
free/bound variables and the second parameter is the type of the free/bound
variable.

\begin{code}
class (Ord x, Ord n) => HasVars x n where
    free :: x -> Set.Set n
    free _ = Set.empty

    occurs :: x -> Set.Set n
    occurs _ = Set.empty

    binders :: x -> [n]
    binders _ = []
\end{code}

\begin{code}
instance HasVars x n => HasVars (Maybe x) n where
    free  Nothing   = Set.empty
    free  (Just x)  = free x

    occurs  Nothing   = Set.empty
    occurs  (Just x)  = occurs x

    binders  Nothing   = []
    binders  (Just x)  = binders x

instance HasVars x n => HasVars [x] n where
    free     x  = foldl' Set.union Set.empty (map free x)
    occurs   x  = foldl' Set.union Set.empty (map occurs x)
    binders  x  = concatMap binders x

instance (HasVars x n, HasVars y n) => HasVars (x, y) n where
    free     (x, y)  = free x `Set.union` free y
    occurs   (x, y)  = occurs x `Set.union` occurs y
    binders  (x, y)  = binders x ++ binders y

instance HasVars x n => HasVars (L x) n where
    free     (L _ x')  = free x'
    occurs   (L _ x')  = occurs x'
    binders  (L _ x')  = binders x'

instance (Ord k, HasVars x n) => HasVars (Map.Map k x) n where
    free m     = Map.fold (\x set -> set `Set.union` free x) Set.empty m
    occurs m   = Map.fold (\x set -> set `Set.union` occurs x) Set.empty m
\end{code}

\begin{code}
data Ord a => Rec a  =  Rec [a]
                     |  NonRec a
  deriving (Eq, Ord)

extractRec :: Ord a => Rec a -> [a]
extractRec  (Rec as)    = as
extractRec  (NonRec a)  = [a]

instance HasVars a n => HasVars (Rec a) n where
    free  (NonRec a)  = free a
    free  (Rec as)    = free as Set.\\ (Set.fromList $ binders as)

    occurs (NonRec a)  = occurs a
    occurs (Rec as)    = occurs as

    binders (NonRec a)  = binders a
    binders (Rec as)    = binders as
\end{code}

\section{Performing Substitutions}

The constraints for the |Subst| class should be read as ``substitute $e$s for
$v$s in $a$.'' We use the substitution algorithm from \emph{Secrets of the
Glasgow Haskell Inliner}~\cite{jones99secrets}, where $\theta$ is the current
substitution and $\phi$ is the set of in-scope variables. Note that this is the
\emph{opposite} convention from that used in the paper!

The second form of substitution, |subste|, is to be used with binders, e.g.,
patterns at let expressions. It returns a modfied $\theta$ and $\phi$ that
should be used for substitutions in the \emph{context} of the binder. Consider
pattern matching in case expressions, in particular a branch |p -> e|. After
performing substitution on |p|, we need to peform substitution on |e| in the
context of |p|. The |subste| form allows us to do this easily.

\begin{code}
class Ord v => CanSubst e v a where
    subste :: Map.Map v e -> Set.Set v -> a -> (Map.Map v e, Set.Set v, a)
    subste theta phi a = (theta, phi, subst theta phi a)

    subst :: Map.Map v e -> Set.Set v -> a -> a
    subst theta phi a = a'
      where
        (_, _, a') = subste theta phi a

    subst1 :: e -> v -> a -> a
    subst1 e v = subst (Map.singleton v e) (Set.singleton v)

instance (Functor f, CanSubst e v a) => CanSubst e v (f a) where
    subste theta phi a = (theta, phi, a')
      where
        a' = fmap (subst theta phi) a

    subst theta phi a = fmap (subst theta phi) a
\end{code}

\begin{code}
class SetLike s a where
    member  :: a -> s -> Bool
    toList  :: s -> [a]

instance Eq a => SetLike [a] a where
    member  = elem
    toList  = id

instance Ord a => SetLike (Set.Set a) a where
    member  = Set.member
    toList  = Set.toList

instance Ord k => SetLike (Map.Map k v) k where
    member  = Map.member
    toList  = Map.keys
\end{code}

\begin{code}
class Freshname a where
    freshname :: SetLike s a => s -> a -> a
\end{code}
