{- 
A single entry stored based on key.  This contains both the user 
data and the metadata associated with the key
-}

import Data.Map (Map)
import qualified Data.Map as Map
import VectorVersion

data EntryMeta = EntryMeta version :: VectorMap

data EntryPayload = EntryPayload 
                        entry :: Map String [Byte]
                        meta :: VectorMap

type Entry = Map String EntryPayload

newEntry :: Entry
newEntry = Map.null

newEntry :: String -> String -> [Byte] -> Entry
newEntry node key entry = Map.fromList [(key, (EntryPayload entry (defaultVector node)))]
