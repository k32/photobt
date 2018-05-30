{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, ScopedTypeVariables #-}
{- Extremely stupid and inefficient implementation of
   row-major sparse matrix indexed by arbitrary 
   instances of Ord class instead of uints
-}
module Data.SparseMaptrix (
                            Maptrix
                          -- , insert
                          -- , updateRow
                          -- , (!)
                          , SlicePlaceholder(..)
                          -- , _
                          , test
                          ) where

import qualified Data.Map as M
import Data.Void
import Data.Traversable

data SlicePlaceholder = SlicePlaceholder

_m = SlicePlaceholder

newtype Maptrix k1 k2 v = Maptrix (M.Map k1 (M.Map k2 v))

type family Slice k1 k2 v i :: * where
  Slice k1 k2 v (k1, SlicePlaceholder) = M.Map k2 v
  Slice k1 k2 v (SlicePlaceholder, k2) = M.Map k1 v
  Slice k1 k2 v (k1, k2) = v

class (Ord k1, Ord k2, Num v) => MSlice k1 k2 v s where
  (!) :: Maptrix k1 k2 v -> s -> Slice k1 k2 v s


instance (Ord k1, Ord k2, Num v) => MSlice k1 k2 v (k1, k2) where
-- instance (Num v) => MSlice Int Int v (Int, Int) where
  (Maptrix m) ! (k1, k2) = case k1 `M.lookup` m of
                             Just x -> case k2 `M.lookup` x of
                                         Just v -> v
                                         Nothing -> 0
                             Nothing -> 0

instance (Ord k1, Ord k2, Num v) => MSlice k1 k2 v (k1, SlicePlaceholder) where
  (Maptrix m) ! (k1, _) = case k1 `M.lookup` m of
                            Just a -> a
                            Nothing -> M.empty

test =
  let m = (Maptrix M.empty) :: Maptrix Int Int Int
      s1 = (m ! (1 :: Int, SlicePlaceholder)) :: M.Map Int Int
      s2 = (m ! (1 :: Int, 2 :: Int)) :: Int
  in
    True
