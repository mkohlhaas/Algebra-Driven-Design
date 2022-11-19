{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scavenge.InputFilter where

import Data.Word
import GHC.Generics
import QuickSpec
import Test.QuickCheck

class HasFilter i where
  data CustomFilter i -- ! 1
  filterMatches :: CustomFilter i -> i -> Bool

------------------------------------------------------------------------------

data InputFilter i
  = Always
  | Never
  | And !(InputFilter i) !(InputFilter i)
  | Or !(InputFilter i) !(InputFilter i)
  | Not !(InputFilter i)
  | Custom !(CustomFilter i)
  deriving stock (Generic)

deriving stock instance (Eq (CustomFilter i)) => Eq (InputFilter i)

deriving stock instance (Ord (CustomFilter i)) => Ord (InputFilter i)

deriving stock instance (Show (CustomFilter i)) => Show (InputFilter i)

-- # ArbitraryInputFilter
instance Arbitrary (CustomFilter i) => Arbitrary (InputFilter i) where
  arbitrary = sized $ \n ->
    if n <= 1
      then elements [always, never]
      else
        frequency
          [ (3, pure always),
            (3, pure never),
            (5, andF <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, orF <$> decayArbitrary 2 <*> decayArbitrary 2),
            (4, notF <$> decayArbitrary 2),
            (8, custom <$> arbitrary)
          ]

  shrink Always = []
  shrink Never = []
  shrink x = Always : Never : genericShrink x

instance
  (Arbitrary i, HasFilter i) =>
  Observe i Bool (InputFilter i)
  where
  observe = flip matches

always :: InputFilter i
always = Always

never :: InputFilter i
never = Never

andF :: InputFilter i -> InputFilter i -> InputFilter i
andF = And

orF :: InputFilter i -> InputFilter i -> InputFilter i
orF = Or

notF :: InputFilter i -> InputFilter i
notF = Not

custom :: CustomFilter i -> InputFilter i
custom = Custom

------------------------------------------------------------------------------

matches :: HasFilter i => InputFilter i -> i -> Bool
matches Always _ = True
matches Never _ = False
matches (And f1 f2) i = matches f1 i && matches f2 i
matches (Or f1 f2) i = matches f1 i || matches f2 i
matches (Not f) i = not $ matches f i
matches (Custom f) i = filterMatches f i

------------------------------------------------------------------------------

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

------------------------------------------------------------------------------

newtype Test
  = Number Word8
  deriving stock (Eq, Ord, Show, Generic)

-- # ArbitraryTest
instance Arbitrary Test where
  arbitrary = Number <$> arbitrary

  shrink = genericShrink

-- # ArbitraryInputTest
instance Arbitrary (CustomFilter Test) where
  arbitrary = Exactly <$> arbitrary

  shrink = genericShrink

exactly :: Word8 -> InputFilter Test
exactly = custom . Exactly

-- # HasFilterTest
instance HasFilter Test where
  data CustomFilter Test = Exactly Word8
    deriving stock (Eq, Ord, Show, Generic)
  filterMatches (Exactly n') (Number n) = n == n'

------------------------------------------------------------------------------

sigFilters :: Sig
sigFilters =
  signature
    [ sigFilterCons,
      sigFilterUserCons,
      sigFilterTypes
    ]

sigFilterCons :: Sig
sigFilterCons =
  signature
    [ con "always" $ always @Test,
      con "never" $ never @Test,
      con "andF" $ andF @Test,
      con "orF" $ orF @Test,
      con "notF" $ notF @Test,
      con "matches" $ matches @Test,
      bools -- ! 1
    ]

sigFilterUserCons :: Sig
sigFilterUserCons =
  signature
    [ con "exactly" exactly,
      con "Number" Number
    ]

sigFilterTypes :: Sig
sigFilterTypes =
  signature
    [ monoVars @(CustomFilter Test) ["f"],
      monoVars @Test ["i"],
      monoVars @Word8 ["n"],
      monoObserve @(InputFilter Test),
      variableUse Linear $ Proxy @(InputFilter Test)
    ]
