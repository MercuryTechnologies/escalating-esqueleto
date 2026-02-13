{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.List (sort)

import Exercises qualified as Exercise
import Answers qualified as Answer
import Types

import Test.Hspec
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig { configColorMode = ColorAlways } $ do
  fdescribe "EE0_StartingOut" $ do
    it "a_iWantToLearnEsqueleto" $ do
      Exercise.a_iWantToLearnEsqueleto `shouldBe` Answer.a_iWantToLearnEsqueleto

  describe "EE1_Select" $ do
    it "a_allFlavors" $ do
      Exercise.a_allFlavors `shouldMatchListRunDB` Answer.a_allFlavors

    it "b_allFlavorNameValues" $ do
      Exercise.b_allFlavorNameValues `shouldMatchListRunDB` Answer.b_allFlavorNameValues

    it "c_allFlavorNames" $ do
      Exercise.c_allFlavorNames `shouldMatchListRunDB` Answer.c_allFlavorNames

    it "d_dairyFreeFlavors" $ do
      Exercise.d_dairyFreeFlavors `shouldMatchListRunDB` Answer.d_dairyFreeFlavors

    it "e_flavorIdsFromNames" $ do
      let neapolitan = ["Chunky Chocolate", "Variegated Vanilla", "Smooth Strawberry"]
      Exercise.e_flavorIdsFromNames neapolitan `shouldMatchListRunDB` Answer.e_flavorIdsFromNames neapolitan

    it "f_customersWithoutBirthdaysWithFlavors" $ do
      Exercise.f_customersWithoutBirthdaysWithFlavors `shouldMatchListRunDB` Answer.f_customersWithoutBirthdaysWithFlavors

  describe "EE2_Join" $ do
    it "a_favoriteFlavors" $ do
      Exercise.a_favoriteFlavors `shouldMatchListRunDB` Answer.a_favoriteFlavors

    it "b_flavorPopularity" $ do
      Exercise.b_flavorPopularity `shouldBeRunDB` Answer.b_flavorPopularity

    it "c_largestGroup" $ do
      Exercise.c_largestGroup `shouldMatchListRunDB` Answer.c_largestGroup

    it "d_customerGroups" $ do
      Exercise.d_customerGroups `shouldMatchMapRunDB` Answer.d_customerGroups

  describe "EE3_TypeZoo" $ do

    -- Because these exercises are at the type level and are pretty complicated, it's probably nicer to check by hand.
    -- Just change False to True when your type signature is correct, to check off that test as completed.

    it "a_runDB" $ do
      checkedByHand False

    it "b_select" $ do
      checkedByHand False

    it "c_val" $ do
      checkedByHand False

    it "d_orderBy" $ do
      checkedByHand False

    -- (?.)
    it "e_project" $ do
      checkedByHand False

    it "f_leftJoin" $ do
      checkedByHand False

  describe "EE4_Errors" $ do
    it "a_getChocolate" $ do
      Exercise.a_getChocolate `shouldBeRunDB` Answer.a_getChocolate

    it "b_flavorNames" $ do
      Exercise.b_flavorNames `shouldMatchListRunDB` Answer.b_flavorNames

    it "c_flavorNameValues" $ do
      Exercise.c_flavorNameValues `shouldMatchListRunDB` Answer.c_flavorNameValues

    it "d_mostPopularFlavor" $ do
      Exercise.d_mostPopularFlavor `shouldBeRunDB` Answer.d_mostPopularFlavor

    it "e_customerPurchases" $ do
      Exercise.e_customerPurchases `shouldMatchListRunDB` Answer.e_customerPurchases

checkedByHand :: Bool -> IO ()
checkedByHand False = expectationFailure "Check this exercise by hand, then change False to True in the tests to mark it as completed."
checkedByHand True = pure ()

shouldBeRunDB :: (Eq a, Show a) => DB a -> DB a -> IO ()
shouldBeRunDB exr ans = do
  exercise <- runDB exr
  answer <- runDB ans
  exercise `shouldBe` answer

shouldMatchListRunDB :: (Eq a, Show a) => DB [a] -> DB [a] -> IO ()
shouldMatchListRunDB exr ans = do
  exercise <- runDB exr
  answer <- runDB ans
  exercise `shouldMatchList` answer

shouldMatchMapRunDB :: (Eq a, Show a, Eq b, Ord b, Show b) => DB [(a, [b])] -> DB [(a, [b])] -> IO ()
shouldMatchMapRunDB exr ans = do
  exercise <- runDB exr
  answer <- runDB ans
  map sortNested exercise `shouldMatchList` map sortNested answer
  where
    sortNested :: Ord b => (a, [b]) -> (a, [b])
    sortNested (x, ys) = (x, sort ys)

-- Used to check this repo's answers. You won't need it if you're doing the exercises.
_developTest :: (Eq a, Show a) => ignored -> DB a -> IO ()
_developTest _ignored ans = do
  let line = putStrLn . take 80 $ cycle "─"
  line
  answer <- runDB ans
  let answerStr = show answer
  putStrLn $ if length answerStr < 1000 then answerStr else take 1000 answerStr <> "... (output truncated)"
  line
