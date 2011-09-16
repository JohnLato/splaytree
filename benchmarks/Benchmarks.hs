{-# LANGUAGE GADTs #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.Bits ((.&.))
import Data.Hashable (Hashable)
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.SplayTree.Set as SS
import qualified Data.Foldable as F
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import qualified Util.ByteString as UBS
import qualified Util.Int as UI
import qualified Util.String as US

instance NFData BS.ByteString

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

instance NFData a => NFData (SS.Set a) where
    rnf s = rnf $ F.toList s

main :: IO ()
main = do
    let hm   = HS.fromList elems :: HS.HashSet String
        hmbs = HS.fromList elemsBS :: HS.HashSet BS.ByteString
        hmi  = HS.fromList elemsI :: HS.HashSet Int
        hmi2 = HS.fromList elemsI2 :: HS.HashSet Int
        m    = Set.fromList elems :: Set.Set String
        mbs  = Set.fromList elemsBS :: Set.Set BS.ByteString
        sm   = SS.fromList elems :: SS.Set String
        smbs = SS.fromList elemsBS :: SS.Set BS.ByteString
        smi  = SS.fromList elemsI :: SS.Set Int
        smi2 = SS.fromList elemsI2 :: SS.Set Int
    defaultMainWith defaultConfig
        (liftIO . evaluate $ rnf [B m, B mbs, B hm, B hmbs, B hmi, B sm, B smbs, B smi, B smi2])
        [
          -- * Comparison to other data structures
          -- ** Set
          bgroup "Set"
          [ bgroup "member"
            [ bench "String" $ whnf (memberM elems) m
            , bench "ByteString" $ whnf (memberM elemsBS) mbs
            ]
          , bgroup "member-miss"
            [ bench "String" $ whnf (memberM elems') m
            , bench "ByteString" $ whnf (memberM elemsBS') mbs
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insertM elems) Set.empty
            , bench "ByteStringString" $ whnf (insertM elemsBS) Set.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insertM elems) m
            , bench "ByteStringString" $ whnf (insertM elemsBS) mbs
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (deleteM elems) m
            , bench "ByteString" $ whnf (deleteM elemsBS) mbs
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (deleteM elems') m
            , bench "ByteString" $ whnf (deleteM elemsBS') mbs
            ]
          , bgroup "size"
            [ bench "String" $ whnf Set.size m
            , bench "ByteString" $ whnf Set.size mbs
            ]
          , bgroup "fromList"
            [ bench "String" $ whnf Set.fromList elems
            , bench "ByteString" $ whnf Set.fromList elemsBS
            ]
          ]
          ,

          -- ** HashSet
          bgroup "HashSet"
          [ bgroup "member"
            [ bench "String" $ whnf (member elems) hm
            , bench "ByteString" $ whnf (member elemsBS) hmbs
            ]
          , bgroup "member-miss"
            [ bench "String" $ whnf (member elems') hm
            , bench "ByteString" $ whnf (member elemsBS') hmbs
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insert elems) HS.empty
            , bench "ByteStringString" $ whnf (insert elemsBS) HS.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insert elems) hm
            , bench "ByteStringString" $ whnf (insert elemsBS) hmbs
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (delete elems) hm
            , bench "ByteString" $ whnf (delete elemsBS) hmbs
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (delete elems') hm
            , bench "ByteString" $ whnf (delete elemsBS') hmbs
            ]
          , bgroup "size"
            [ bench "String" $ whnf HS.size hm
            , bench "ByteString" $ whnf HS.size hmbs
            ]
          , bgroup "fromList"
            [ bench "String" $ whnf HS.fromList elems
            , bench "ByteString" $ whnf HS.fromList elemsBS
            ]
          ]

          -- * Basic interface
        , bgroup "Splay"
          [
            bgroup "member"
            [ bench "String" $ whnf (memberSp elems) sm
            , bench "ByteString" $ whnf (memberSp elemsBS) smbs
            , bench "Int" $ whnf (memberSp elemsI) smi
            ]
          , bgroup "member-32"
            [ bench "String" $ whnf (memberSp elems32) sm
            , bench "ByteString" $ whnf (memberSp elemsBS32) smbs
            , bench "Int" $ whnf (memberSp elemsI32) smi
            ]
          , bgroup "member-miss"
            [ bench "String" $ whnf (memberSp elems') sm
            , bench "ByteString" $ whnf (memberSp elemsBS') smbs
            , bench "Int" $ whnf (memberSp elemsI') smi
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insertSp elems) SS.empty
            , bench "ByteString" $ whnf (insertSp elemsBS) SS.empty
            , bench "Int" $ whnf (insertSp elemsI) SS.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insertSp elems) sm
            , bench "ByteString" $ whnf (insertSp elemsBS) smbs
            , bench "Int" $ whnf (insertSp elemsI) smi
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (deleteSp elems) sm
            , bench "ByteString" $ whnf (deleteSp elemsBS) smbs
            , bench "Int" $ whnf (deleteSp elemsI) smi
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (deleteSp elems') sm
            , bench "ByteString" $ whnf (deleteSp elemsBS') smbs
            , bench "Int" $ whnf (deleteSp elemsI') smi
            ]
  
            -- Combine
          , bench "union" $ whnf (SS.union smi) smi2
  
            -- Transformations
          , bench "map" $ whnf (SS.map (\ v -> v + 1)) smi

            -- Folds
          , bench "foldl'" $ whnf (F.foldl' (+) 0) smi
          , bench "foldr" $ whnf (F.foldr (:) []) smi

            -- Filter
          -- , bench "filter" $ whnf (SS.filter (\ v -> v .&. 1 == 0)) smi

            -- Size
          , bgroup "size"
            [ bench "String" $ whnf SS.size sm
            , bench "ByteString" $ whnf SS.size smbs
            , bench "Int" $ whnf SS.size smi
            ]

            -- fromList
          , bgroup "fromList"
            [ bench "String" $ whnf SS.fromList elems
            , bench "ByteString" $ whnf SS.fromList elemsBS
            , bench "Int" $ whnf SS.fromList elemsI
            ]
          ]
        ]
  where
    n :: Int
    n = 2^(12 :: Int)

    elems   = US.rnd 8 n
    elemsBS = UBS.rnd 8 n
    elemsI  = UI.rnd (n+n) n

    elems32 = take n $ cycle $ take 32 elems
    elemsBS32 = take n . cycle $ take 32 elemsBS
    elemsI32  = take n . cycle $ take 32 elemsI

    elems'   = US.rnd 8 n
    elemsBS' = UBS.rnd 8 n
    elemsI'  = UI.rnd (n+n) n

    elemsI2  = UI.rnd (n+n) n

------------------------------------------------------------------------
-- * SplaySet

memberSp :: (Eq k, Ord k) => [k] -> SS.Set k -> k
memberSp xs m = snd $ foldl' f (m, head xs) xs
 where
  f (m', k) z = case SS.memberSplay k m' of
     (True,  m'2) -> (m'2, k)
     (False, m'2) -> (m'2, z)
{-# INLINE memberSp #-}

insertSp :: (Eq k, Ord k) => [k] -> SS.Set k
       -> SS.Set k
insertSp xs m0 = foldl' (\m v -> SS.insert v m) m0 xs
{-# INLINE insertSp #-}

deleteSp :: (Eq k, Ord k) => [k] -> SS.Set k -> SS.Set k
deleteSp xs m0 = foldl' (\m k -> SS.delete k m) m0 xs

------------------------------------------------------------------------
-- * HashSet

member :: (Eq k, Hashable k) => [k] -> HS.HashSet k -> k
member xs m = foldl' (\z k -> if HS.member k m then k else z) (head xs) xs

insert :: (Eq k, Hashable k) => [k] -> HS.HashSet k
       -> HS.HashSet k
insert xs m0 = foldl' (\m v -> HS.insert v m) m0 xs

delete :: (Eq k, Hashable k) => [k] -> HS.HashSet k -> HS.HashSet k
delete xs m0 = foldl' (\m k -> HS.delete k m) m0 xs

------------------------------------------------------------------------
-- * Set

memberM :: (Ord k) => [k] -> Set.Set k -> k
memberM xs m = foldl' (\z k -> if Set.member k m then k else z) (head xs) xs

insertM :: Ord k => [k] -> Set.Set k -> Set.Set k
insertM xs m0 = foldl' (\m v -> Set.insert v m) m0 xs

deleteM :: Ord k => [k] -> Set.Set k -> Set.Set k
deleteM xs m0 = foldl' (\m k -> Set.delete k m) m0 xs
