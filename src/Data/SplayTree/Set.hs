{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor
            ,DeriveFoldable
            ,DeriveTraversable #-}

module Data.SplayTree.Set (
  module S
 ,Set
 ,empty
 ,null
 ,size
 ,member
 ,memberSplay
 ,insert
 ,delete
 ,union
 ,map
 ,fromList
)

where

import           Prelude hiding (null, map)
import qualified Prelude as P
import           Data.SplayTree (SplayTree (..), Measure (..), (|>), (<|), (><), query, fmap')
import qualified Data.SplayTree as S

import           Control.Applicative hiding (empty)
import           Data.Maybe
import           Data.Monoid
import           Data.Foldable
import           Data.Traversable

-- a Set type
data Elem a = None | Elem a
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

instance (Ord a) => Monoid (Elem a) where
  mempty = None
  mappend None b = b
  mappend a None = a
  mappend a b = (max a b)

instance (Ord a) => Measured (Elem a) where
  type Measure (Elem a) = Elem a
  measure a = a

newtype Set a = Set { unSet :: SplayTree (Elem a) }
  deriving (Eq, Show, Ord, Foldable)

instance Ord a => Monoid (Set a) where
  mempty = Set mempty
  mappend = union

empty :: (Ord a) => Set a
empty = Set S.empty

null :: (Ord a) => Set a -> Bool
null = S.null . unSet

size :: (Ord a) => Set a -> Int
size = S.size . unSet

member :: (Ord a) => a -> Set a -> Bool
member a (Set tree) = case snd <$> query (>= Elem a) tree of
  Nothing                  -> False
  Just (S.Branch _ l a' r) -> Elem a == a'

-- | Check if @a@ is a member, and return a set splayed to @a@.
-- The return set is splayed to an element near @a@ if @a@ isn't in the
-- set.
memberSplay :: (Ord a) => a -> Set a -> (Bool, Set a)
memberSplay a (Set tree) = case snd <$> query (>= Elem a) tree of
  Nothing                      -> (False, Set tree)
  Just foc@(S.Branch _ l a' r) -> (Elem a == a', Set foc)

fromList :: (Ord a) => [a] -> Set a
fromList = Set . S.fromList . P.map Elem

insert :: (Ord a) => a -> Set a -> Set a
insert a (Set tree) = case snd <$> query (>= aIn) tree of
  Nothing -> Set $ tree |> aIn
  Just foc@(S.Branch _ l a' r) -> if aIn == a'
    then Set foc
    else Set $ l >< (aIn <| a' <| r)
 where aIn = Elem a

delete :: (Ord a) => a -> Set a -> Set a
delete a (Set tree) = case snd <$> query (>= Elem a) tree of
  Nothing -> Set tree
  Just foc@(S.Branch _ l a' r) -> if Elem a == a'
    then Set (l >< r)
    else Set foc

union :: (Ord a) => Set a -> Set a -> Set a
union l r = foldl' (flip insert) l r

map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map f = Set . fmap' (fmap f) . unSet

