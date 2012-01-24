{-
	An implementation of Vector Versioning (or Vector Clocks) for use in 
	versionging results of the store.
-}

module VectorVersion (
	  Vector
	, VectorMap
	, getConflicts
	, incrementVersion
	, defaultVector) where

	import qualified Data.Map as Map


	type Vector a = (a,Int)
	type VectorMap k = Map.Map k Int

	hasConflicts :: VectorMap a -> VectorMap a -> Bool
	hasConflicts v1 v2 = undefined

	getConflicts :: VectorMap a -> VectorMap a -> VectorMap a
	getConflicts v1 v2 = undefined

	resolveConflicts :: VectorMap a -> VectorMap a
	resolveConflicts v = undefined

	incrementVersion :: Vector a -> Vector a
	incrementVersion (a,b) = (a, b + 1)

	toVectorMap :: Ord a => [Vector a] -> VectorMap a
	toVectorMap vList = Map.fromList vList

	defaultVector :: a -> Vector a
	defaultVector a = (a,0)

