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
 ,lookupAt
)

where

import Data.SplayTree (Measured (..), SplayTree (..), fmap', traverse', (<|))
import qualified Data.SplayTree as S

import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Foldable
import Data.Traversable

-- a Seq type
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Ord, Eq, Num, Enum, Functor, Foldable, Traversable)

instance Measured (Elem a) where
  type Measure (Elem a) = Sum Int
  measure _ = Sum 1

newtype Seq a = Seq { unSeq :: SplayTree (Elem a) }
  deriving (Eq, Show, Ord, Foldable, Monoid)

instance Functor Seq where
  fmap f = Seq . fmap' (fmap f) . unSeq

instance Traversable Seq where
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
empty = Seq $ S.empty

-- | Look up a value at the given index.  Returns that value
-- if it exists, and the appropriately splayed Seq.
lookupAt :: Seq a -> Int -> (Maybe a, Seq a)
lookupAt (Seq tree) ix | ix < 0 = (Nothing, Seq (S.deepL tree))
lookupAt (Seq tree) ix = case S.query (>= Sum (ix+1)) tree of
  Just (elem, tree') -> (Just $ getElem elem, Seq tree')
  Nothing            -> (Nothing, Seq (S.deepR tree))
{-# INLINE lookupAt #-}
