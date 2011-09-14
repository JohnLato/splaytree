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
 ,difference
 ,intersection
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

-- | Construct an empty set
empty :: (Ord a) => Set a
empty = Set S.empty

-- | Construct a set with a single element
singleton :: (Ord a) => a -> Set a
singleton = Set . S.singleton . Elem

-- | 'True' if this set is empty, 'False' otherwise.
null :: (Ord a) => Set a -> Bool
null = S.null . unSet

-- | Return the number of elements in this set.
size :: (Ord a) => Set a -> Int
size = S.size . unSet

-- | Return 'True' if the given value is present in this set, 'False' otherwise.
member :: (Ord a) => a -> Set a -> Bool
member a set = fst $ memberSplay a set

-- | Check if @a@ is a member, and return a set splayed to @a@.
-- The return set is splayed to an element near @a@ if @a@ isn't in the
-- set.
memberSplay :: (Ord a) => a -> Set a -> (Bool, Set a)
memberSplay a (Set tree) = fmap Set $ S.memberSplay (Elem a) tree

-- | Construct a @Set@ from a list of elements.
-- 
-- The Set is created by calling 'Data.SplayTree.fromListBalance'.
fromList :: (Ord a) => [a] -> Set a
fromList = Set . S.fromListBalance . P.map Elem

-- | Add the specified value to this set.
insert :: (Ord a) => a -> Set a -> Set a
insert a = Set . S.insert (Elem a) . unSet

-- | Remove the specified value from this set if present.
delete :: (Ord a) => a -> Set a -> Set a
delete a (Set tree) = Set $ S.delete (Elem a) tree

-- | Construct a set containing all elements from both sets.
--
-- The smaller set should be presented as the second argument.
union :: (Ord a) => Set a -> Set a -> Set a
union l r = foldl' (flip insert) l r

-- | Transform this set by applying a function to every value.
map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map f = fromList . P.map f . toList

-- | Difference of two sets.  Contains elements of the first set that are
-- not present in the second.
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set l) (Set r) = Set (S.difference l r)

-- | Intersection of two sets.  Contains all elements which are in both
-- sets.
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set l) (Set r) = Set (S.intersection l r)
