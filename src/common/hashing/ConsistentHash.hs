{-
    A consistent hashing algorithm that is used to distribute keys amoung nodes
    Based, mostly, on the example from 
    http://www.fatvat.co.uk/2010/02/consistent-hashing-with-haskell.html
-}

module ConsistentHash where

import Data.Maybe
import Data.Map (Map)
import Data.Digest.Pure.SHA
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (sort,delete,(\\),findIndex,(!!))
import Control.Applicative

type Node = String
type Ring = Map Int Node

data HashRing = HashRing Int Ring [Node] [Int]


emptyRing :: Int -> HashRing
emptyRing i = HashRing i Map.empty [] []

createRing :: [Node] -> Int -> HashRing
createRing nodes replicas = foldl addNode (emptyRing replicas) nodes

addNode :: HashRing -> Node -> HashRing
addNode (HashRing rep ring nodes skeys) node = HashRing rep newRing newNodes (sort newKeys) where
    (newRing,nweKeys) = foldl (addEntry node) (ring,skeys) [0 .. rep]
    newNodes = node: nodes

addEntry :: Node -> (Ring,[Int]) -> Int -> (Ring,[Int])
addEntry n (r,ks) i = (nr, key : ks) where
    key = hashNodeId n i
    nr  = Map.insert key n r

hashNodeId :: Node -> Int -> Int
hashNodeId node i = hashString h where
    h = "#{" ++ node ++ "}:#{" ++ show i ++ "}"

hashString :: String -> Int
hashString s = (fromInteger (integerDigest (sha1 (pack s))) :: Int)

removeNode :: HashRing -> Node -> HashRing
removeNode (HashRing rep ring nodes skeys) node = HashRing rep newRing newNodes newKeys where
    (newRing,newKeys) = foldl (removeEntry node) (ring,skeys) [0 .. rep]
    newNodes = delete node nodes

removeEntry :: Node -> (Ring,[Int]) -> Int -> (Ring,[Int])
removeEntry n (r,ks) i = (nr, ks \\ [key]) where
    key = hashNodeId n i
    nr = Map.delete key r

getNode :: HashRing -> String -> Maybe Node
getNode hashRing k | isNothing x = Nothing
                   | otherwise = Just (fst (fromJust x))
getNode hashRing k = fst <$> getNodePos hashRing (hashString k) 

getNodePos :: HashRing -> Int -> Maybe (Node,Int)
getNodePos (HashRing _ ring _ skeys) ksy | Map.null ring  = Nothing
                                         | otherwise = Just (x,y)
    where 
        y = fromMaybe 0 (findIndex (>= key) skeys)
        x = ring Map.! (skeys !! y)