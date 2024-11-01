module EE01_allFlavorNameValues where

import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

-- Note again that this generates SQL to select a single column
-- and doesn't filter "after the fact" in Haskell
allFlavorNameValues :: DB [Value Text]
allFlavorNameValues = do
  select $ do
    flavor <- from $ table @Flavor
    pure $ flavor.name
