{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor
            ,DeriveFoldable
            ,DeriveTraversable #-}

module Data.SplayTree.Set (
  module S
 ,Set
)

where

import Data.SplayTree as S

import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable

-- a Set type
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Ord, Eq, Num, Enum, Functor, Foldable, Traversable)

instance (Ord a, Bounded a) => Monoid (Elem a) where
  mempty = Elem (minBound)
  mappend a b = (max a b)

instance (Ord a, Bounded a) => Measured (Elem a) where
  type Measure (Elem a) = Elem a
  measure a = a

newtype Set a = Set { unSet :: SplayTree (Elem a) }
  deriving (Eq, Show, Ord, Foldable, Monoid)
