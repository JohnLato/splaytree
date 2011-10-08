{-# LANGUAGE DeriveDataTypeable
            ,DeriveFunctor
            ,FlexibleContexts
            ,FlexibleInstances
            ,GeneralizedNewtypeDeriving
            ,TypeFamilies
            ,GADTs
            ,BangPatterns
            ,UndecidableInstances #-}

module Data.SplayTree (
  SplayTree (..)
 ,Measured (..)
 ,empty
 ,(|>)
 ,(<|)
 ,(><)
 ,null
 ,singleton
 ,size
 ,split
 ,query
 ,memberSplay
 ,delete
 ,insert
 ,difference
 ,intersection
 ,balance
 ,deepL
 ,deepR
 ,fromList
 ,fromListBalance
 ,fmap'
 ,traverse'
)

where

import Prelude hiding (foldr, null)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.DeepSeq

import Data.Data
import Data.Foldable
import Data.Maybe
import Data.Monoid

infixr 5 ><
infixr 5 <|
infixl 5 |>

{-# INLINE (><) #-}
{-# INLINE (<|) #-}
{-# INLINE (|>) #-}


class Monoid (Measure a) => Measured a where
  type Measure a :: *
  measure :: a -> Measure a

data SplayTree a where
  Tip :: SplayTree a
  Branch :: (Measure a) -> (SplayTree a) -> !a -> (SplayTree a) -> SplayTree a
 deriving (Typeable)

instance (NFData a, NFData (Measure a)) => NFData (SplayTree a) where
  rnf Tip = ()
  rnf (Branch m l a r) = m `deepseq` l `deepseq` a `deepseq` rnf r 

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
branch l a r = Branch mm l a r
 where
  mm = case (l,r) of
    (Tip, Tip) -> measure a
    (Tip, Branch rm _ _ _) -> measure a `mappend` rm
    (Branch lm _ _ _, Tip) -> lm `mappend` measure a
    (Branch lm _ _ _, Branch rm _ _ _) -> mconcat [lm, measure a, rm]

instance Foldable SplayTree where
  foldMap _ Tip              = mempty
  foldMap f (Branch _ l a r) = mconcat [foldMap f l, f a, foldMap f r]
  {-# INLINE foldMap #-}
  foldl = myFoldl
  {-# INLINE foldl #-}

myFoldl :: (a -> b -> a) -> a -> SplayTree b -> a
myFoldl f i0 tree = go i0 tree
 where
  go !i    Tip = i
  go !acc (Branch _ l a r) = let a1 = go acc l
                                 a2 = a1 `seq` f a1 a
                             in a2 `seq` go a2 r
{-# INLINE myFoldl #-}

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

-- | /O(n)/.  Create a Tree from a finite list of elements.
fromList :: (Measured a) => [a] -> SplayTree a
fromList = foldl' (|>) Tip

-- | /O(n)/.  Create a Tree from a finite list of elements.
-- 
-- After the tree is created, it is balanced.  This is useful with sorted data,
-- which would otherwise create a completely unbalanced tree.
fromListBalance :: (Measured a) => [a] -> SplayTree a
fromListBalance = balance . fromList

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
  desc i (b@(Branch _ Tip a Tip), zp) = (a,b,zp)
  desc i (b@(Branch _ Tip a r), zp)
    | p mm = (a,b,zp)
    | otherwise = desc mm $ fromJust (descendR b zp)
   where mm = i `mappend` measure a
  desc i (b@(Branch _ l a r), zp)
    | p ml = desc i $ fromJust (descendL b zp)
    | p mm = (a,b,zp)
    | otherwise = desc mm $ fromJust (descendR b zp)
   where
    ml = i `mappend` measure l
    mm = ml `mappend` measure a
{-# INLINE query #-}

-- --------------------------
-- Basic interface

size :: SplayTree a -> Int
size = foldl' (\acc _ -> acc+1) 0

memberSplay
  :: (Measured a, Ord (Measure a), Eq a)
  => a
  -> SplayTree a
  -> (Bool, SplayTree a)
memberSplay a tree = case snd <$> query (>= (measure a)) tree of
  Nothing -> (False, tree)
  Just foc@(Branch _ l a' r) -> (a == a', foc)
{-# INLINE memberSplay #-}

delete
  :: (Measured a, Ord (Measure a), Eq a)
  => a
  -> SplayTree a
  -> SplayTree a
delete a tree = case memberSplay a tree of
  (False, t') -> t'
  (True, Branch _ l _ r) -> l >< r

insert 
  :: (Measured a, Ord (Measure a), Eq a)
  => a
  -> SplayTree a
  -> SplayTree a
insert a tree = case snd <$> query (>= measure a) tree of
  Nothing -> tree |> a
  Just t'@(Branch _ l a' r) -> if a == a'
    then t'
    else l >< (a <| a' <| r)

-- --------------------------
-- Set operations

difference
  :: (Measured a, Ord (Measure a), Eq a)
  => SplayTree a
  -> SplayTree a
  -> SplayTree a
difference l r = foldl' (flip delete) l r

intersection
  :: (Measured a, Ord (Measure a), Eq a)
  => SplayTree a
  -> SplayTree a
  -> SplayTree a
intersection l r = fst $ foldl' f (empty, l) r
 where
  f (acc,testSet) x = case memberSplay x testSet of
    (True,  t') -> (insert x acc, t')
    (False, t') -> (acc, t')

-- --------------------------
-- Traversals

-- | Like fmap, but with a more restrictive type.
fmap' :: Measured b => (a -> b) -> SplayTree a -> SplayTree b
fmap' f Tip = Tip
fmap' f (Branch _ l a r) = branch (fmap' f l) (f a) (fmap' f r)

-- | Like traverse, but with a more restrictive type.
traverse'
  :: (Measured b, Applicative f)
  => (a -> f b)
  -> SplayTree a
  -> f (SplayTree b)
traverse' f Tip = pure Tip
traverse' f (Branch _ l a r) =
  branch <$> traverse' f l <*> f a <*> traverse' f r

-- | descend to the deepest left-hand branch
deepL :: Measured a => SplayTree a -> SplayTree a
deepL = deep descendL

-- | descend to the deepest right-hand branch
deepR :: Measured a => SplayTree a -> SplayTree a
deepR = deep descendR

-- | Descend a tree using the provided `descender` descending function,
-- then recreate the tree.  The new focus will be the last node accessed
-- in the tree.
deep
  :: Measured a
  => (SplayTree a -> [Thread a] -> Maybe (SplayTree a, [Thread a]))
  -> SplayTree a
  -> SplayTree a
deep descender tree = uncurry ascendSplay . desc $ descender tree []
 where
  desc (Just (Tip, zp))          = (Tip, zp)
  desc (Just (b@(Branch{}), zp)) = desc $ descender b zp
  desc Nothing                   = (tree, [])
{-# INLINE deep #-}

-- -------------------------------------------
-- splay tree stuff...

-- use a zipper so descents/splaying can be done in a single pass
data Thread a = DescL !a !(SplayTree a)
              | DescR !a !(SplayTree a)

descendL :: SplayTree a -> [Thread a] -> Maybe (SplayTree a, [Thread a])
descendL (Branch _ l a r) zp = Just (l, DescL a r : zp)
descendL _  _                = Nothing

descendR :: SplayTree a -> [Thread a] -> Maybe (SplayTree a, [Thread a])
descendR (Branch _ l a r) zp = Just (r, DescR a l : zp)
descendR _  _                = Nothing

up :: Measured a => SplayTree a -> Thread a -> SplayTree a
up tree (DescL a r) = branch tree a r
up tree (DescR a l) = branch l a tree
{-# INLINE up #-}

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
  go !x [] = x
  go !x zp = uncurry go $ ascendSplay' x zp

ascendSplay' :: Measured a => SplayTree a -> [Thread a] -> (SplayTree a, [Thread a])
ascendSplay' !x (pt@(DescL{}) : gt@(DescL{}) : zp') =
  let g = up (up x pt) gt in (rotateL (rotateL g), zp')
ascendSplay' !x (pt@(DescR{}) : gt@(DescR{}) : zp') =
  let g = up (up x pt) gt in (rotateR (rotateR g), zp')
ascendSplay' !x (pt@(DescR{}) : gt@(DescL{}) : zp') =
  (rotateL $ up (rotateR (up x pt)) gt, zp')
ascendSplay' !x (pt@(DescL{}) : gt@(DescR{}) : zp') =
  (rotateR $ up (rotateL (up x pt)) gt, zp')
ascendSplay' !x [pt@(DescL{})] = (rotateL (up x pt), [])
ascendSplay' !x [pt@(DescR{})] = (rotateR (up x pt), [])
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
