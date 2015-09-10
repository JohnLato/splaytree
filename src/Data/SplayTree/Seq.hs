{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor
            ,DeriveFoldable
            ,DeriveTraversable #-}

module Data.SplayTree.Seq (
  Seq
 ,cons
 ,Data.SplayTree.Seq.toList
 ,fromList
 ,empty
 ,length
 ,lookupAt
 ,init
)

where

import Prelude hiding (length, init)

import Data.SplayTree (Measured (..), SplayTree (..), fmap', traverse', (<|))
import qualified Data.SplayTree as S

import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Foldable hiding (length)
import Data.Traversable

-- a Seq type
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Ord, Eq, Num, Enum, Functor, Foldable, Traversable)

instance Measured (Elem a) where
  type Measure (Elem a) = Sum Int
  {-# INLINE measure #-}
  measure _ = Sum 1

newtype Seq a = Seq { unSeq :: SplayTree (Elem a) }
  deriving (Eq, Show, Ord, Foldable, Monoid)

instance Functor Seq where
  {-# INLINE fmap #-}
  fmap f = Seq . fmap' (fmap f) . unSeq

instance Traversable Seq where
  {-# INLINE traverse #-}
  traverse f = fmap Seq . traverse' (traverse f) . unSeq

cons :: a -> Seq a -> Seq a
cons a = Seq . (Elem a <|) . unSeq
{-# INLINE cons #-}

toList :: Seq a -> [a]
toList = Data.Foldable.toList
{-# INLINE toList #-}

fromList :: [a] -> Seq a
fromList = Seq . S.fromListBalance . map Elem
{-# INLINE fromList #-}

empty :: Seq a
empty = Seq S.empty

length :: Seq a -> Int
length (Seq tree) = case S.deepR tree of
  Branch m _ _ _  -> getSum m
  Tip             -> 0
-- could use the Seq.size function, but since the Measure is keeping track
-- of size anyway, this seems cleaner.  Also probably more efficient.
{-# INLINE length #-}

-- | Look up a value at the given index.  Returns that value
-- if it exists, and the appropriately splayed Seq.
lookupAt :: Seq a -> Int -> (Maybe a, Seq a)
lookupAt (Seq tree) ix | ix < 0 = (Nothing, Seq (S.deepL tree))
lookupAt (Seq tree) ix = case S.query (>= Sum (ix+1)) tree of
  Just (elem, tree') -> (Just $ getElem elem, Seq tree')
  Nothing            -> (Nothing, Seq (S.deepR tree))
{-# INLINE lookupAt #-}

init :: Seq a -> Seq a
init (Seq tree) = case S.deepR tree of
  Branch _ l _ Tip -> Seq l
  Tip              -> Seq Tip
  _                -> error "splayTree: internal error in Seq.init."
{-# INLINE init #-}
