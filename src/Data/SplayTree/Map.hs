{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor
            ,DeriveFoldable
            ,DeriveTraversable #-}

module Data.SplayTree.Map (
  Map
 ,Data.SplayTree.Map.toList
 ,fromList
 ,empty
 ,S.size
 ,insert
 ,insertWith
 ,delete
 ,lookup
 ,init
)

where

import Prelude hiding (length, init)

import Data.SplayTree (Measured (..), SplayTree (..), fmap', traverse', (<|),
                       query, (><), (|>))
import qualified Data.SplayTree as S

import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Foldable
import Data.Traversable

-- a Max monoid.  There used to be one in the monoids package, but that's
-- now obsolete and I can't find a replacement, so it's going here...

newtype Max a = Max { unMax :: Maybe a } deriving (Eq, Show, Ord)

instance (Ord a) => Monoid (Max a) where
  mempty = Max Nothing
  mappend (Max l) (Max r) = case (l,r) of
      (Just l', Just r') -> Max (Just (max l' r'))
      (Just _,  Nothing) -> Max l
      (Nothing, _)       -> Max r

-- a strict Map type
data Elem k a = Elem { eKey :: !k
                     , eVal :: !a
                     }
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

unElem :: Elem k a -> (k,a)
unElem (Elem k a) = (k,a)

instance Ord k => Measured (Elem k a) where
  type Measure (Elem k a) = Max k
  {-# INLINE measure #-}
  measure = Max . Just . eKey

type Map k a = SplayTree (Elem k a)

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = insertWith (flip const)

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith fn key val tree = case snd <$> query (>= Max (Just key)) tree of
    Just (S.Branch _ l oElem r) -> if eKey oElem == key
        then l >< ( (Elem key (fn (eVal oElem) val)) <| r)
        else l >< ( (Elem key val) <| r)
    Nothing -> tree |> Elem key val

delete :: (Ord k) => k -> Map k a -> Map k a
delete key tree = case snd <$> query (>= Max (Just key)) tree of
  Nothing -> tree
  Just tree'@(S.Branch _ l oElem r) -> if eKey oElem == key
      then l >< r
      else tree'

toList :: Map k a -> [(k,a)]
toList = map unElem . Data.Foldable.toList
{-# INLINE toList #-}

fromList :: Ord k => [(k,a)] -> Map k a
fromList = S.fromListBalance . map (uncurry Elem)
{-# INLINE fromList #-}

empty :: Map k a
empty = S.empty


-- | Look up a value at the given index.  Returns that value
-- if it exists, and the appropriately splayed Map.
lookupAt :: Ord k => Map k a -> k -> (Maybe a, Map k a)
lookupAt (tree) key = case query (>= Max (Just key)) tree of
  Just (elem, tree') -> if eKey elem == key
                          then (Just $ eVal elem, tree')
                          else (Nothing, tree')
  Nothing            -> (Nothing, S.deepR tree)
{-# INLINE lookupAt #-}

init :: Ord k => Map k a -> Map k a
init (tree) = case S.deepR tree of
  Branch _ l _ Tip -> l
  Tip              -> Tip
  _                -> error "splayTree: internal error in Map.init."
{-# INLINE init #-}
