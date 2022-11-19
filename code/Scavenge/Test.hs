{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scavenge.Test where

import Data.List (sortOn)
import Data.MultiSet (MultiSet, fromList, toList)
import Data.Semigroup.Cancellative (Commutative)
import QuickSpec (Arbitrary, Observe)
import Test.QuickCheck (Arbitrary (..), Gen, genericShrink)

type TestReward = MultiSet Int

instance Commutative TestReward

instance Arbitrary TestReward where
  arbitrary ∷ Gen TestReward
  arbitrary = fromList <$> arbitrary
  shrink ∷ TestReward → [TestReward]
  shrink = fmap fromList . sortOn length . genericShrink . toList

instance Observe () TestReward TestReward

type TestClue = Int
