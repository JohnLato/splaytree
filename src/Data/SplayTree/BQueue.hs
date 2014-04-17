{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor
            ,DeriveFoldable
            ,DeriveTraversable #-}

{-# OPTIONS -fprof-auto-top #-}

module Data.SplayTree.BQueue (
  BQueue
 ,cons
 ,Data.SplayTree.BQueue.toList
 ,Data.SplayTree.BQueue.foldl
 ,fromList
 ,elems
 ,empty
 ,length
 ,lookup
 ,init
 ,insert
 ,adjust
 ,delete
 ,find
)

where

import Prelude hiding (lookup, length, init)

import Data.SplayTree (Measured (..), SplayTree (..), fmap', traverse', (<|),(><), (|>))
import qualified Data.SplayTree as S

import Control.Arrow (first)
import Control.Applicative hiding (empty)
import Data.Monoid
import qualified Data.Foldable as Fold
import Data.Traversable

-- a Seq type
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Ord, Eq, Num, Enum, Functor, Fold.Foldable, Traversable)

instance Measured (Elem a) where
  type Measure (Elem a) = Sum Int
  {-# INLINE measure #-}
  measure _ = Sum 1

newtype BQueue a = BQueue { unBQueue :: SplayTree (Elem a) }
  deriving (Eq, Show, Ord, Fold.Foldable, Monoid)

instance Functor BQueue where
  {-# INLINE fmap #-}
  fmap f = BQueue . fmap' (fmap f) . unBQueue

instance Traversable BQueue where
  {-# INLINE traverse #-}
  traverse f = fmap BQueue . traverse' (traverse f) . unBQueue

cons :: a -> BQueue a -> BQueue a
cons a = BQueue . (Elem a <|) . unBQueue
{-# INLINE cons #-}

toList :: BQueue a -> [a]
toList = Fold.toList
{-# INLINE toList #-}

fromList :: [a] -> BQueue a
fromList = BQueue . S.fromListBalance . map Elem
{-# INLINE fromList #-}

empty :: BQueue a
empty = BQueue S.empty

length :: BQueue a -> Int
length (BQueue tree) = case S.deepR tree of
  Branch m _ _ _  -> getSum m
  Tip             -> 0
-- could use the Seq.size function, but since the Measure is keeping track
-- of size anyway, this seems cleaner.  Also probably more efficient.
{-# INLINE length #-}

-- internally, a BQueue is 1-indexed, but we want to expose a 0-indexed API.
-- Therefore we add 1 to every user-specified index.

-- | Look up a value at the given index.  Returns that value
-- if it exists, and the appropriately splayed BQueue.
lookup :: BQueue a -> Int -> (Maybe a, BQueue a)
lookup (BQueue tree) ix | ix < 0 = (Nothing, BQueue (S.deepL tree))
lookup (BQueue tree) ix = case S.query (>= Sum (ix+1)) tree of
  Just (elem, tree') -> (Just $ getElem elem, BQueue tree')
  Nothing            -> (Nothing, BQueue (S.deepR tree))
{-# INLINE lookup #-}

(!) :: BQueue a -> Int -> (a, BQueue a)
q ! ix = first (maybe (error "BQueue.!: not found!") id) $ lookup q ix

init :: BQueue a -> BQueue a
init (BQueue tree) = case S.deepR tree of
  Branch _ l _ Tip -> BQueue l
  Tip              -> BQueue Tip
  _                -> error "splayTree: internal error in BQueue.init."
{-# INLINE init #-}

singleton :: a -> BQueue a
singleton = BQueue . S.singleton . Elem

insert :: Int -> a -> BQueue a -> BQueue a
insert ix x (BQueue tree) = case S.query (>= Sum (ix+1)) tree of
    Just (_, Branch _ l a' r) -> BQueue $ l >< (Elem x <| a' <| r)
    Nothing -> BQueue $ tree |> Elem x
{-# INLINE insert #-}

adjust :: (a -> a) -> Int -> BQueue a -> BQueue a
adjust f ix (BQueue tree) = case snd <$> S.query (>= Sum (ix+1)) tree of
    Just (Branch _ l a r) -> BQueue $ l >< ((f <$> a) <| r)
    Nothing -> BQueue tree
{-# INLINE adjust #-}

delete :: Int -> BQueue a -> BQueue a
delete ix (BQueue tree) = case snd <$> S.query (>= Sum (ix+1)) tree of
    Nothing -> BQueue tree
    Just (Branch _ l _ r) -> BQueue $ l >< r
{-# INLINE delete #-}

null :: BQueue a -> Bool
null (BQueue Tip) = True
null _            = False

elems :: BQueue a -> [a]
elems = Data.SplayTree.BQueue.toList

-- find does a linear-like search, so we really don't want to splay the
-- tree
find :: (a -> Bool) -> BQueue a -> Maybe (Int, a)
find p (BQueue tree) = go 0 tree
  where
    go _ Tip = Nothing
    go pre (Branch m l (Elem a) r) = case go pre l of
        Nothing | p a -> Just (pre+getSum (measure l), a)
                | otherwise ->
                    let sum' = pre + 1 + getSum (measure l)
                    in sum' `seq` go sum' r
        Just r -> return r

foldl :: (a -> b -> a) -> a -> BQueue b -> a
foldl = Fold.foldl
