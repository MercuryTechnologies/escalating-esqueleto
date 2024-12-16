{-# OPTIONS_GHC -fdefer-type-errors #-}

module EE4_Errors where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

{-
A fairly common complaint from beginners to esqueleto is that the type errors
it produces can be hard to decipher. Practice reading the type errors generated
by the queries below, and fix the queries so they work as intended.

The queries may have multiple issues. Even if you spot an issue immediately,
try to fully understand what each type error is telling you, before fixing that
specific problem and moving on to the next.
-}

a_getChocolate :: DB (Maybe (Entity Flavor))
a_getChocolate = do
  selectOne $ do
    flavor <- from $ table @Flavor
    where_ $ flavor.name == "Chunky Chocolate"
    pure flavor

b_flavorNames :: DB [Text]
b_flavorNames = do
  select $ do
    flavor <- from $ table @Flavor
    pure $ unValue flavor.name

-- also check out the error message in this version of the last exercise:
-- b2_flavorNames :: DB [Text]
-- b2_flavorNames = do
--   select $ do
--     flavor <- from $ table @Flavor
--     pure $ fmap unValue flavor.name

c_flavorNameValues :: DB [Value Text]
c_flavorNameValues = do
  flavors <- select $ from $ table @Flavor
  pure $ map (\f -> f.name) flavors

d_mostPopularFlavor :: DB (Maybe FlavorId)
d_mostPopularFlavor = do
  selectOne $ do
    (_customer :& flavor) <- from $
      table @Customer `innerJoin` table @Flavor
      `on` (\(customer :& flavor) -> customer.favoriteFlavor ==. flavor.id)
    groupBy flavor.id
    orderBy [desc countRows]
    pure flavor.id

e_customerPurchases :: DB [(CustomerId, Dollar)]
e_customerPurchases = do
  fmap coerce $ select $ do
    purchase <- from $ table @Purchase
    groupBy purchase.customerId
    pure (purchase.customerId, purchase.amount)
