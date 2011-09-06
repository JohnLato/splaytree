{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor
            ,DeriveFoldable
            ,DeriveTraversable #-}

module Data.SplayTree.Seq (
  module S
 ,Seq
)

where

import Data.SplayTree as S

import Control.Applicative
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
