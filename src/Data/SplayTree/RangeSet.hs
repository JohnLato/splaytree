{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            ,DeriveFunctor
            ,DeriveFoldable
            ,DeriveTraversable #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.SplayTree.RangeSet (
  Range (..)
 ,RangeSet
 ,point
 ,range
 ,rangePs
 ,inRange
 ,rangeMax
 ,null
 ,singleton
 ,empty
 ,append
 ,insert
 ,delete
 ,fromList
)

where

import           Prelude hiding (null)
import qualified Prelude as P
import           Data.SplayTree ((><), (|>), (<|), Measure (..), query)
import qualified Data.SplayTree as S

import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Foldable
import Data.Traversable

-- | a RangeSet element
data Range a = Range {
  rMin  :: !a
 ,rang  :: !a
 }
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

data RangeM a = NoRange | RangeM (Range a) deriving (Show, Ord, Eq)

instance (Num a, Ord a) => Monoid (RangeM a) where
  mempty            = NoRange
  mappend NoRange b = b
  mappend a NoRange = a
  mappend (RangeM lr@(Range lm lrng)) (RangeM rr@(Range rm rrng)) =
    let rEnd = max (rangeMax lr) (rangeMax rr)
        rSt  = min lm rm
    in  RangeM $ Range rSt (rEnd - rSt)

instance (Num a, Ord a) => Measured (Range a) where
  type Measure (Range a) = RangeM a
  measure a = RangeM a

type RangeSet a = S.SplayTree (Range a)
  -- deriving (Eq, Show, Ord, Foldable, Monoid)

-- | A range of a single point (range =0)
point :: Num a => a -> Range a
point x = Range x 0

-- | Create a @Range@ from a minimum value and range
range :: (Num a, Ord a) => a -> a -> Range a
range start rng
  | rng >= 0 = Range start rng
  | otherwise = error "Range must have a non-negative range"

-- | Create a @Range@ from the two endpoints.
rangePs :: (Num a, Ord a) => a -> a -> Range a
rangePs start stop = let mn = min start stop
                         dst = max start stop - mn
                     in Range mn dst

-- | check if a value is within the range
inRange :: (Num a, Ord a) => a -> Range a -> Bool
inRange x (Range mn r) = x >= mn && x <= mn+r

rangeMax :: (Num a) => Range a -> a
rangeMax (Range mn r) = mn+r

compareRange :: (Num a, Ord a) => Range a -> RangeM a -> Bool
compareRange _ NoRange     = False
compareRange r (RangeM r') = rMin r <= rangeMax r'

-- | create a range which encompasses both provided ranges
combineMax :: (Num a, Ord a) => Range a -> Range a -> Range a
combineMax l r = let min' = min (rMin l) (rMin r)
                     rng' = (max (rangeMax l) (rangeMax r)) - min'
                 in Range min' rng'

-- | subtract the second range from the first
subRange :: (Num a, Ord a) => Range a -> Range a -> [Range a]
subRange m@(Range mMin mDur) s@(Range sMin sDur)
  | sMin > mMin && sMax < mMax   = [Range mMin (sMin - mMin)
                                   ,Range sMax (mMax - sMax)]
  | sMin > mMax || sMax < mMin   = [m]
  | sMin <= mMin && sMax >= mMax = []
  | sMin <= mMin                 = [Range sMax mMax]
  | otherwise                    = [Range mMin sMin]
 where
  sMax = rangeMax s
  mMax = rangeMax m

-- -----------------------------------------------
-- RangeSet ops

empty :: RangeSet a
empty = S.empty

singleton :: (Num a, Ord a) => Range a -> RangeSet a
singleton r = r <| S.empty

null :: RangeSet a -> Bool
null = S.null

fromList :: (Num a, Ord a) => [Range a] -> RangeSet a
fromList = foldl' insert empty

append :: (Num a, Ord a) => RangeSet a -> RangeSet a -> RangeSet a
append l r = foldl' insert l (toList r)

insert :: (Num a, Ord a) => RangeSet a -> Range a -> RangeSet a
insert tree rng = case snd <$> query (compareRange rng) tree of
  Nothing                      -> tree S.|> rng
  Just (S.Branch _ l oRange r) -> if rMin oRange > rangeMax rng
    then l >< (rng <| oRange <| r)
    else l >< insert r (combineMax oRange rng)

delete :: (Num a, Ord a) => RangeSet a -> Range a -> RangeSet a
delete tree rng = case snd <$> query (compareRange rng) tree of
  Nothing -> tree
  Just (S.Branch _ l oRange r) -> case subRange oRange rng of
    []  -> l >< delete r rng
    [x] -> l >< (x <| delete r rng)
    xs  -> l >< S.fromList xs >< delete r rng

