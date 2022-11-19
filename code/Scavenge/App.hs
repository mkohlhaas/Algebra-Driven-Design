{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scavenge.App where

import Data.Foldable
import qualified Data.Map.Monoidal as M
import Data.MultiSet hiding (empty)
import QuickSpec
import Scavenge.ClueState
import Scavenge.Initial
import Scavenge.InputFilter
import Scavenge.Test
import Test.QuickCheck

sig ∷ Sig
sig =
  signature
    [ sigCons,
      sigTypes,
      sigMonoid
    ]

sigMonoid ∷ Sig
sigMonoid =
  background
    [ con "mempty" $ liftC @(Monoid A) $ mempty @A,
      con "<>" $ liftC @(Semigroup A) $ (<>) @A
    ]

sigCons ∷ Sig
sigCons =
  signature
    [ con "both" $ both @Test @TestClue @TestReward,
      con "eitherC" $ eitherC @Test @TestClue @TestReward,
      con "empty" $ empty @Test @TestClue @TestReward,
      con "clue" $ clue @Test @TestClue @TestReward,
      con "andThen" $ andThen @Test @TestClue @TestReward,
      con "reward" $ reward @Test @TestClue @TestReward,
      con "gate" $ gate @Test @TestClue @TestReward,
      con "bottom" $ bottom @Test @TestClue @TestReward
    ]

-- TODO(sandy): write about this?
sigObs ∷ Sig
sigObs =
  series
    [ sigCons,
      lists,
      signature
        [ con "stateOf" $ \c k is →
            M.lookup [k] $ getClues @Test @TestClue @TestReward c is,
          con "prune" $ \k →
            eitherC @Test @TestClue @TestReward empty (clue k bottom),
          con "Just" $ Just @ClueState,
          con "Nothing" $ Nothing @ClueState,
          con "seen" seen,
          con "failed" failed,
          con "completed" completed
        ],
      signature
        [ con "getRewards" $ getRewards @Test @TestClue @TestReward
        ]
    ]

sigOpts ∷ Sig
sigOpts =
  signature
    [ variableUse Linear $ -- ! 1
        Proxy @(Challenge Test TestClue TestReward),
      withMaxTermSize 6
    ]

sigTestOpts ∷ Sig
sigTestOpts =
  signature
    [ withMaxTermSize 7,
      withMaxTests 1000000,
      withMaxTestSize 40,
      withPrintStyle ForQuickCheck
    ]

sigTypes ∷ Sig
sigTypes =
  signature
    [ monoObserve @(Challenge Test TestClue TestReward),
      vars ["c"] $
        Proxy @(Challenge Test TestClue TestReward),
      monoObserve @TestReward,
      vars ["r"] $ Proxy @TestReward,
      monoObserve @(InputFilter Test),
      vars ["f"] $ Proxy @(InputFilter Test),
      monoVars @(CustomFilter Test) ["f"],
      monoVars @TestClue ["k"],
      monoVars @Test ["i"],
      instanceOf @(Monoid [TestClue]), -- ! 1
      instanceOf @(Semigroup [TestClue]), -- ! 1
      instanceOf @(Monoid TestReward),
      instanceOf @(Semigroup TestReward),
      mono @(Maybe ClueState),
      mono @ClueState
    ]

#include "spec.inc"

main ∷ IO ()
-- main = traverse_ (quickCheck . uncurry counterexample) quickspec_laws
main = traverse_ (quickCheck . uncurry counterexample) quickspecLaws
