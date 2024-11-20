module EE04_customerPurchases where

import Data.Coerce (coerce)
import Database.Esqueleto.Experimental
import Schema
import Types

customerPurchases :: DB [(CustomerId, Dollar)]
customerPurchases = do
  fmap coerce $ select $ do
    purchase <- from $ table @Purchase

    groupBy purchase.customerId

    -- Depending on your interpretation of this query's intent, you can either remove the groupBy above,
    -- or add an aggregate function (like `sum_`) to `purchase.amount` in the line below

    pure (purchase.customerId, coalesceDefault [sum_ purchase.amount] $ val (0.0 :: Dollar))
