{-# LANGUAGE DeriveDataTypeable
            ,DeriveFunctor
            ,FlexibleContexts
            ,FlexibleInstances
            ,GeneralizedNewtypeDeriving
            ,TypeFamilies
            ,GADTs
            ,UndecidableInstances #-}

module Data.SplayTree (
  SplayTree
 ,Measured (..)
 ,empty
 ,(|>)
 ,(<|)
 ,(><)
 ,null
 ,split
 ,query
 ,fmap'
 ,traverse'
 ,balance
)

where

import Prelude hiding (foldr, null)
import Control.Applicative hiding (empty)
import Control.Monad

import Data.Data
import Data.Foldable
import Data.Maybe
import Data.Monoid

infixr 5 ><
infixr 5 <|
infixl 5 |>


class Monoid (Measure a) => Measured a where
  type Measure a :: *
  measure :: a -> Measure a

data SplayTree a where
  Tip :: SplayTree a
  Branch :: Measure a -> SplayTree a -> a -> SplayTree a -> SplayTree a
 deriving (Typeable)

instance (Eq a) => Eq (SplayTree a) where
  xs == ys = toList xs == toList ys

instance (Ord a) => Ord (SplayTree a) where
  compare xs ys = compare (toList xs) (toList ys)

instance (Show a, Show (Measure a)) => Show (SplayTree a) where
  show Tip = "Tip"
  show (Branch v l a r) = "Branch {ann {" ++ show v ++ "}, lChild {" ++ show l ++ "}, value {" ++ show a ++ "}, rChild {" ++ show r ++ "}}"

instance Measured a => Monoid (SplayTree a) where
  mempty  = Tip
  mappend = (><)

instance (Measured a) => Measured (SplayTree a) where
  type Measure (SplayTree a) = Measure a
  measure Tip              = mempty
  measure (Branch v _ _ _) = v

leaf :: Measured a => a -> SplayTree a
leaf a = Branch (measure a) Tip a Tip

branch :: Measured a => SplayTree a -> a -> SplayTree a -> SplayTree a
branch l a r = Branch (mconcat [measure l, measure a, measure r]) l a r

instance Foldable SplayTree where
  foldMap _ Tip              = mempty
  foldMap f (Branch _ l a r) = mconcat [foldMap f l, f a, foldMap f r]

-- -------------------------------------------
-- Construction

empty :: SplayTree a
empty = Tip

singleton :: Measured a => a -> SplayTree a
singleton = leaf

(<|) :: (Measured a) => a -> SplayTree a -> SplayTree a
a <| Tip          = branch Tip a Tip
a <| t@(Branch{}) = asc . desc $ descendL t []
 where
  asc = uncurry ascendSplay
  desc (Just (Tip, zp))         = (leaf a, zp)
  desc (Just (b@(Branch {}), zp)) = desc $ descendL b zp
  desc Nothing                    = error "SplayTree.(<|): internal error"

(|>) :: (Measured a) => SplayTree a -> a -> SplayTree a
Tip          |> b = leaf b
t@(Branch{}) |> b = asc . desc $ descendR t []
 where
  asc = uncurry ascendSplay
  desc (Just (Tip, zp))         = (leaf b, zp)
  desc (Just (b@(Branch {}), zp)) = desc $ descendR b zp
  desc Nothing                    = error "SplayTree.(|>): internal error"

-- | Append two trees.
(><) :: (Measured a) => SplayTree a -> SplayTree a -> SplayTree a
Tip >< ys  = ys
xs  >< Tip = xs
l   >< r = asc . desc $ descendL r []
 where
  asc = uncurry ascendSplay
  desc (Just (Tip, zp))          = (l, zp)
  desc (Just (b@(Branch{}), zp)) = desc $ descendL b zp
  desc Nothing                   = error "SplayTree.(><): internal error"

-- | /O(n)/.  Create a Tree from a finite list of elements.  Currently
-- this function should be avoided as it constructs an unbalanced tree.
fromList :: (Measured a) => [a] -> SplayTree a
fromList = foldr (<|) Tip

-- -------------------------------------------
-- deconstruction

-- | Is the tree empty?
null :: SplayTree a -> Bool
null Tip = True
null _     = False

-- | Split a tree at the point where the predicate on the measure changes from
-- False to True.
split
  :: Measured a
  => (Measure a -> Bool)
  -> SplayTree a
  -> (SplayTree a, SplayTree a)
split _p Tip = (Tip, Tip)
split p  tree = case query p tree of
  Just (_, Branch _ l a r) -> (l, (branch Tip a r))
  _                        -> (Tip, Tip)

-- | find the first point where the predicate returns True.  Returns a tree
-- splayed with that node at the top.
query
  :: (Measured a, Measure a ~ Measure (SplayTree a))
  => (Measure a -> Bool)
  -> SplayTree a
  -> Maybe (a, SplayTree a)
query p t
  | p (measure t) = Just . asc $ desc mempty (t, [])
  | otherwise = Nothing
 where
  asc (a,t',zp) = (a, ascendSplay t' zp)
  desc i (b@(Branch _ l a r), zp)
    | p ml = desc i $ fromJust (descendL b zp)
    | p mm = (a,b,zp)
    | otherwise = desc mm $ fromJust (descendR b zp)
   where
    ml = i `mappend` measure l
    mm = ml `mappend` measure a

-- | Like fmap, but with a more restrictive type.
fmap' :: Measured b => (a -> b) -> SplayTree a -> SplayTree b
fmap' f Tip = Tip
fmap' f (Branch _ l a r) = branch (fmap' f l) (f a) (fmap' f r)

traverse'
  :: (Measured b, Applicative f)
  => (a -> f b)
  -> SplayTree a
  -> f (SplayTree b)
traverse' f Tip = pure Tip
traverse' f (Branch _ l a r) =
  branch <$> traverse' f l <*> f a <*> traverse' f r

-- -------------------------------------------
-- splay tree stuff...

-- use a zipper so descents/splaying can be done in a single pass
data Thread a = DescL a (SplayTree a)
              | DescR a (SplayTree a)

descendL :: SplayTree a -> [Thread a] -> Maybe (SplayTree a, [Thread a])
descendL (Branch _ l a r) zp = Just (l, DescL a r : zp)
descendL _  _                = Nothing

descendR :: SplayTree a -> [Thread a] -> Maybe (SplayTree a, [Thread a])
descendR (Branch _ l a r) zp = Just (r, DescR a l : zp)
descendR _  _                = Nothing

up :: Measured a => SplayTree a -> Thread a -> SplayTree a
up tree (DescL a r) = branch tree a r
up tree (DescR a l) = branch l a tree

rotateL :: (Measured a) => SplayTree a -> SplayTree a
rotateL (Branch annP (Branch annX lX aX rX) aP rP) =
  branch lX aX (branch rX aP rP)
rotateL tree = tree

-- actually a left rotation, but calling it a right rotation matches with
-- the descent terminology
rotateR :: (Measured a) => SplayTree a -> SplayTree a
rotateR (Branch annP lP aP (Branch annX lX aX rX)) =
  branch (branch lP aP lX) aX rX
rotateR tree = tree

ascendSplay :: Measured a => SplayTree a -> [Thread a] -> SplayTree a
ascendSplay x zp = go x zp
 where
  go x [] = x
  go x zp = uncurry go $ ascendSplay' x zp

ascendSplay' :: Measured a => SplayTree a -> [Thread a] -> (SplayTree a, [Thread a])
ascendSplay' x (pt@(DescL{}) : gt@(DescL{}) : zp') =
  let g = up (up x pt) gt in (rotateL (rotateL g), zp')
ascendSplay' x (pt@(DescR{}) : gt@(DescR{}) : zp') =
  let g = up (up x pt) gt in (rotateR (rotateR g), zp')
ascendSplay' x (pt@(DescR{}) : gt@(DescL{}) : zp') =
  (rotateL $ up (rotateR (up x pt)) gt, zp')
ascendSplay' x (pt@(DescL{}) : gt@(DescR{}) : zp') =
  (rotateR $ up (rotateL (up x pt)) gt, zp')
ascendSplay' x [pt@(DescL{})] = (rotateL (up x pt), [])
ascendSplay' x [pt@(DescR{})] = (rotateR (up x pt), [])
ascendSplay' _ [] = error "SplayTree: internal error, ascendSplay' called past root"

-- ---------------------------
-- A measure of tree depth
newtype ElemD a = ElemD { getElemD :: a } deriving (Show, Ord, Eq, Num, Enum)

newtype Depth = Depth {getDepth :: Int}
  deriving (Show, Ord, Eq, Num, Enum, Real, Integral)

instance Monoid Depth where
  mempty = 0
  (Depth l) `mappend` (Depth r) = Depth (max l r)

instance Measured (ElemD a) where
  type Measure (ElemD a) = Depth
  measure _ = 1

-- | rebalance a splay tree.  The order of elements does not change.
balance :: Measured a => SplayTree a -> SplayTree a
balance = fmap' getElemD . balance' . fmap' ElemD

balance' :: SplayTree (ElemD a) -> SplayTree (ElemD a)
balance' Tip = Tip
balance' (Branch _ l a r) =
  let l' = balance' l
      r' = balance' r
      diff   = measure l' - measure r'
      numRots = fromIntegral $ diff `div` 2
      b' = Branch (mconcat [1+measure l', measure a, 1+measure r']) l' a r'
  in case (numRots > 0, numRots < 0) of
      (True, _) -> iterate rotateL b' !! numRots
      (_, True) -> iterate rotateR b' !! abs numRots
      otherwise -> b'
