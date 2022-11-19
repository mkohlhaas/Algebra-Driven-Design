{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scavenge.Results where

import Data.Map.Monoidal (MonoidalMap, fromList, toList)
import GHC.Generics
import Generic.Data
import QuickSpec
import Scavenge.ClueState
import Test.QuickCheck

data Results k r = Results
  { rewards ∷ !r,
    clues ∷ !(MonoidalMap [k] ClueState)
  }
  deriving stock (Eq, Ord, Generic)
  deriving
    (Semigroup, Monoid)
    via Generically (Results k r)

instance (Show k, Show r) ⇒ Show (Results k r) where
  show (Results r k) =
    mconcat
      [ "Results (",
        show r,
        ") (fromList ",
        show $ toList k,
        ")"
      ]

instance
  (Arbitrary k, Ord k, Arbitrary v) ⇒
  Arbitrary (MonoidalMap k v)
  where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . genericShrink . toList

instance (Ord k, Ord v) ⇒ Observe () (MonoidalMap k v) (MonoidalMap k v)

instance
  (Ord k, Ord r) ⇒
  Observe () (Results k r) (Results k r)
