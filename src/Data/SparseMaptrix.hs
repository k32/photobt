{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{- Extremely stupid and inefficient implementation of
   row-major sparse matrix indexed by arbitrary 
   instances of Ord class instead of uints
-}
module Data.SparseMaptrix (
                            Maptrix
                          , Slice(..)
                          , update
                          , H(..)
                          , S(..)
                          , test
                          ) where

import qualified Data.Map.Strict as M
import Data.Void
import Data.Traversable

data H = H

newtype S a = S a

newtype Maptrix k1 k2 v = Maptrix (M.Map k1 (M.Map k2 v))

type family Slice k1 k2 v i :: * where
  Slice k1 k2 v (S k1, H) = M.Map k2 v
  Slice k1 k2 v (H, S k2) = M.Map k1 v
  Slice k1 k2 v (S k1, S k2) = v

class (Ord k1, Ord k2, Num v) => MSlice k1 k2 v s where
  (!) :: Maptrix k1 k2 v    -- ^ Map
      -> s                  -- ^ Index 
      -> Slice k1 k2 v s

  insert :: s               -- ^ Index
         -> Slice k1 k2 v s -- ^ Value to insert
         -> Maptrix k1 k2 v -- ^ Initial map
         -> Maptrix k1 k2 v

instance (Ord k1, Ord k2, Num v) => MSlice k1 k2 v (S k1, S k2) where
-- instance (Num v) => MSlice Int Int v (Int, Int) where
  (Maptrix m) ! (S k1, S k2) = case k1 `M.lookup` m of
                                 Just x -> case k2 `M.lookup` x of
                                             Just v -> v
                                             Nothing -> 0
                                 Nothing -> 0

  insert (S k1, S k2) elem (Maptrix m) =
      Maptrix $ M.insertWith (const $ M.insert k2 elem)
                             k1
                             (M.singleton k2 elem)
                             m

instance (Ord k1, Ord k2, Num v) => MSlice k1 k2 v (S k1, H) where
  (Maptrix m) ! (S k1, _) = case k1 `M.lookup` m of
                            Just a -> a
                            Nothing -> M.empty
                                       
  insert (S k1, H) row (Maptrix m) = Maptrix $ M.insert k1 row m

update :: (Ord k1, Ord k2, Num v)
       => Maptrix k1 k2 v
       -> [(k1, [(k2, v)])]
       -> Maptrix k1 k2 v
update = undefined

test =
  let m = (Maptrix M.empty) :: Maptrix String String Int
      s1 = (m ! (S "", S "1")) :: Int
      s2 = (m ! (S "", H)) :: M.Map String Int
  in
    True
