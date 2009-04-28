{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}

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
-- Module      :  Data.Loc
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Data.Loc where

import Data.Generics
import Data.List (sort)

-- | Position type.
data Pos = Pos String               -- ^file name
               {-# UNPACK #-} !Int  -- ^line
               {-# UNPACK #-} !Int  -- ^column
               {-# UNPACK #-} !Int  -- ^character offset
  deriving(Eq, Data, Typeable)

instance Ord Pos where
    compare (Pos f1 l1 c1 _) (Pos f2 l2 c2 _) =
        compare (f1, l1, c1) (f2, l2, c2)

posFile :: Pos -> String
posFile (Pos f _ _ _) = f

posLine :: Pos -> Int
posLine (Pos _ l _ _) = l

posCol :: Pos -> Int
posCol (Pos _ _ c _) = c

posCoff :: Pos -> Int
posCoff (Pos _ _ _ coff) = coff

-- | Location type, consisting of a beginning position and an end position.
data Loc = Loc {-# UNPACK #-} !Pos  -- |beginning
               {-# UNPACK #-} !Pos  -- |end
  deriving(Eq, Ord, Data, Typeable)

locStart :: Loc -> Loc
locStart (Loc p _) = Loc p p

locEnd :: Loc -> Loc
locEnd (Loc _ p) = Loc p p

startPos :: Loc -> Pos
startPos (Loc p _) = p

endPos :: Loc -> Pos
endPos (Loc p _) = p

-- | Source location type. Source location are all equal, which allows AST nodes
-- to be compared modulo location information.
newtype SrcLoc = SrcLoc Loc
  deriving(Data, Typeable)

instance Eq SrcLoc where
    _ == _ = True

instance Ord SrcLoc where
    compare _ _ = EQ

internalPos :: Pos
internalPos = Pos "<internal>" (-1) 0 0

internalLoc :: Location a => a
internalLoc = fromLoc $ Loc internalPos internalPos

builtinPos :: Pos
builtinPos = Pos "<builtin>" (-1) 0 0

builtinLoc :: Location a => a
builtinLoc = fromLoc $ Loc builtinPos builtinPos

combineLocs :: [Loc] -> Loc
combineLocs locs =
    Loc (head ps) (last ps)
  where
    ps :: [Pos]
    ps = sort $ extractPs locs

    extractPs :: [Loc] -> [Pos]
    extractPs  []               = []
    extractPs  (Loc p1 p2 : l)  = p1 : p2 : extractPs l

infixl 6 <-->

(<-->)  ::  (Located a, Located b)
        =>  a -> b -> Loc
x <--> y = Loc (min b1 b2) (max e1 e2)
  where
    (Loc b1 e1) = getLoc x
    (Loc b2 e2) = getLoc y

-- | Locations
class Location a where
    fromLoc :: Loc -> a
    toLoc :: a -> Loc

instance Location Pos where
    fromLoc (Loc pos _)  = pos
    toLoc pos = Loc pos pos

instance Location Loc where
    fromLoc  = id
    toLoc    = id

instance Location SrcLoc where
    fromLoc = SrcLoc
    toLoc (SrcLoc loc) = loc

-- | Located objects have a location.
class Located a where
    getLoc :: a -> Loc

instance Located a => Located [a] where
    getLoc xs = foldr1 (<-->) (map getLoc xs)

instance Located Pos where
    getLoc p = Loc p p

instance Located Loc where
    getLoc = id

-- | Locations are ignored when performing comparisons.
data L x = L Loc x

instance Eq x => Eq (L x) where
    (L _ x) == (L _ y) = x == y

instance Ord x => Ord (L x) where
    compare (L _ x) (L _ y) = compare x y

instance Located (L a) where
    getLoc (L loc _) = loc

unLoc :: L a -> a
unLoc (L _ a) = a

instance Show x => Show (L x) where
    show (L _ x) = show x
