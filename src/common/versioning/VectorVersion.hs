{-
    An implementation of Vector Versioning (or Vector Clocks) for use in 
    versionging results of the store.
-}

module VectorVersion (
      Vector
    , VectorMap
    , (\\?)
    , incrementVersion
    , defaultVector) where

import Data.Map (Map,(\\))
import qualified Data.Map as Map


type Vector a = (a,Integer)
type VectorMap k = Map.Map k Integer

{- 
    A conflict occurs when v2 contains either new nodes,
    or a version that is < a node on v1.  One exception would
    be a version of 1 on a new node (which means the first version
    of the object from the new node).
-}
hasConflicts :: Ord a => VectorMap a -> VectorMap a -> Bool
hasConflicts v1 v2 = (Map.size v1 > Map.size v2) || case (v1 \\ v2) of 
        empty     -> vectorsAreDescendents v1 v2
        new       -> vectorsAreNotNew new
    
    
vectorsAreNotNew :: Ord a => VectorMap a -> Bool
vectorsAreNotNew m = any (> 1) (Map.elems m)

vectorsAreDescendents :: Ord a => VectorMap a -> VectorMap b -> Bool
vectorsAreDescendents v1 v2 = any (\(a,b) -> b < a) (zip (Map.elems v1) (Map.elems v2))

(\\?) :: Ord a => VectorMap a -> VectorMap a -> Bool
v1 \\? v2 = hasConflicts v1 v2

incrementVersion :: Vector a -> Vector a
incrementVersion (a,b) = (a, b + 1)

toVectorMap :: Ord a => [Vector a] -> VectorMap a
toVectorMap vList = Map.fromList vList

defaultVector :: a -> Vector a
defaultVector a = (a,0)

