{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, TypeFamilies #-}
{- Extremely stupid and inefficient implementation of
   row-major sparse matrix indexed by arbitrary
   instances of Ord class instead of uints
-}
module Data.SparseMaptrix (
                            Maptrix
                          , Slice(..)
                          , MSlice(..)
                          , update
                          , updateWith
                          , H(..)
                          , S(..)
                          , new
                          , toList
                          , fromList
                          , rows
                          ) where

import qualified Data.Map.Strict as M
import Data.Void
import Data.Traversable
import Data.List (foldl', groupBy, sortBy)
import Data.Function (on)

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

instance (Ord k1, Ord k2, Eq v, Num v) => MSlice k1 k2 v (S k1, S k2) where
-- instance (Num v) => MSlice Int Int v (Int, Int) where
  (Maptrix m) ! (S k1, S k2) = case k1 `M.lookup` m of
                                 Just x -> case k2 `M.lookup` x of
                                             Just v -> v
                                             Nothing -> 0
                                 Nothing -> 0

  insert (S k1, S k2) 0 (Maptrix m) = Maptrix $ M.update f k1 m
    where f m = case M.delete k2 m of
                  m' | M.null m' -> Nothing
                     | otherwise -> Just m'
  insert (S k1, S k2) elem (Maptrix m) =
    Maptrix $ M.insertWith (const $ M.insert k2 elem)
                           k1
                           (M.singleton k2 elem)
                           m

new :: Maptrix k1 k2 v
new = Maptrix $ M.empty

instance (Ord k1, Ord k2, Num v) => MSlice k1 k2 v (S k1, H) where
  (Maptrix m) ! (S k1, _) = case k1 `M.lookup` m of
                            Just a -> a
                            Nothing -> M.empty

  insert (S k1, H) row (Maptrix m) = Maptrix $ M.insert k1 row m


updateWith :: (Ord k1, Ord k2, Eq v, Num v)
           => (v -> v -> v)
           -> Maptrix k1 k2 v
           -> [((k1, k2), v)]
           -> Maptrix k1 k2 v
updateWith f (Maptrix m0) l =
  let
    nullToNothing 0 = Nothing
    nullToNothing n = Just n

    groups = groupBy ((==) `on` (fst . fst)) $ sortBy (compare `on` fst) l
    groups' = [(k1, l') | g <- groups
                        , let k1 = fst $ fst $ head g
                              l' = [(k2, v) | ((_, k2), v) <- g]
                        ]

    doRow l row = if M.null row'
                  then Nothing
                  else Just row'
      where row' = foldr doElem row l -- ineffective

    doElem (k2, v) = M.update (\v0 -> nullToNothing $ f v0 v) k2
  in
    Maptrix $ foldr (\(k1, l) -> M.update (doRow l) k1) m0 groups'

update :: (Ord k1, Ord k2, Eq v, Num v)
       => Maptrix k1 k2 v
       -> [((k1, k2), v)]
       -> Maptrix k1 k2 v
update = updateWith (\_ a -> a)

toList :: (Ord k1, Ord k2)
       => Maptrix k1 k2 v
       -> [((k1, k2), v)]
toList (Maptrix m) = [ ((k1, k2), v)
                     | (k1, vv) <- M.toList m
                     , (k2, v) <- M.toList vv
                     ]

fromList :: (Ord k1, Ord k2, Eq v, Num v)
         => [((k1, k2), v)]
         -> Maptrix k1 k2 v
fromList = foldl' go new
  where go m ((k1, k2), v) = insert (S k1, S k2) v m

rows :: (Ord k1, Ord k2, Eq v, Num v)
     => Maptrix k1 k2 v
     -> [k1]
rows (Maptrix m) = M.keys m

test =
  let m = (Maptrix M.empty) :: Maptrix String String Int
      s1 = (m ! (S "", S "1")) :: Int
      s2 = (m ! (S "", H)) :: M.Map String Int
  in
    True
