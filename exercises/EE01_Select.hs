module EE01_Select where

import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types
import Data.Time.Calendar (Day, showGregorian)

{-
Congratulations on becoming the new proprietor of the local Stonecold's ice
cream parlor franchise. I'm from the corporate office and will just have a few
questions as you're getting started. Since I'm more on the business side, I
know SQL well...but Haskell is a bridge too far.

Using esqueleto, can you get me everything we know about our flavors?

The equivalent SQL would be:
SELECT * FROM flavors;
-}
allFlavors :: DB [Entity Flavor]
allFlavors =
  select . from $ table @Flavor

{-
Actually I just want the flavor name values. That would be:
SELECT flavors.name FROM flavors;

Ensure you do this flavor->name projection in Haskell, not after the fact in SQL.
-}
allFlavorNameValues :: DB [Value Text]
allFlavorNameValues =
  select $ do
    flavors <- from $ table @Flavor
    pure $ flavors ^. FlavorName

{-
Both queries above return lists of wrapped types. 'Entity' comes from persistent,
and can be unwrapped into its components via 'entityKey' and 'entityVal'.

'Value' comes from esqueleto. Can you remove the 'Value' wrapper to return a
plain '[Text]'? Start by copying the previous query.

-- I feel like if someone didn't know about 'unValue' that they may have a hard
-- time finding it
-}
allFlavorNames :: DB [Text]
allFlavorNames = do
  flavorNames <- select $ do
    flavors <- from $ table @Flavor
    pure $ flavors ^. FlavorName
  pure $ map unValue flavorNames

{-
Let's introduce WHERE clauses.
A vegan just walked in. Provide all our dairy-free flavors.
-}
dairyFreeFlavors :: DB [Entity Flavor]
dairyFreeFlavors =
  select $ do
    flavors <- from $ table @Flavor
    where_ (flavors ^. FlavorDairyFree ==. val True)
    pure flavors

{-
We'd like to run a mildly nefarious targeted ad campaign. What are the emails
of all our customers who haven't provided their birthday, but have
provided a favorite flavor?

Fill in the type as well.
-}
customersWithoutBirthdaysWithFlavors :: DB [Email]
customersWithoutBirthdaysWithFlavors = do
  emailVals <- select $ do
    customers <- from $ table @Customer
    where_ $ (isNothing $ customers ^. CustomerBirthday)
      &&. (not_ . isNothing $ customers ^. CustomerFavoriteFlavor)
    pure $ customers ^. CustomerEmail
  pure $ map unValue emailVals

{-
Our founder's birthday is April 17th, and we're running a special.
Who are our customers with a 4/17 birthday?

Again, for efficiency you should do all filtering in SQL,
not after the fact in Haskell.

This one is much harder than the exercises above. Feel free to add more imports.
If you need a hint, see hints/EE01_customersSharingFoundersBirthday.md
-}
customersSharingFoundersBirthday :: DB [Entity Customer]
customersSharingFoundersBirthday = do
  let founderBday = "04-17"
      compareBdays :: Day -> Bool
      compareBdays = (==) founderBday . drop 5 . showGregorian
  select $ do
    customers <- from $ table @Customer
    --where_
    pure customers
