{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Arithmetic.Lte
  ( -- * Special Inequalities
    zero
  , reflexive
    -- * Substitution
  , substituteL
  , substituteR
    -- * Increment
  , incrementL
  , incrementR
    -- * Weaken
  , weakenL
  , weakenR
    -- * Composition
  , transitive
  , plus
    -- * Convert Strict Inequality
  , fromStrict
    -- * Integration with GHC solver
  , constant
  ) where

import Arithmetic.Unsafe (type (<)(Lt),type (:=:)(Eq))
import Arithmetic.Unsafe (type (<=)(Lte))
import GHC.TypeNats (CmpNat,type (+))
import qualified GHC.TypeNats as GHC

-- | Replace the right-hand side of a strict inequality
-- with an equal number.
substituteL :: (b :=: c) -> (a < b) -> (a < c)
substituteL Eq Lt = Lt

-- | Replace the right-hand side of a strict inequality
-- with an equal number.
substituteR :: (b :=: c) -> (a < b) -> (a < c)
substituteR Eq Lt = Lt

-- | Add two inequalities.
plus :: (a <= b) -> (c <= d) -> (a + c <= b + d)
plus Lte Lte = Lte

-- | Compose two inequalities using transitivity.
transitive :: (a <= b) -> (b <= c) -> (a <= c)
transitive Lte Lte = Lte

-- | Any number is less-than-or-equal-to itself.
reflexive :: a <= a
reflexive = Lte

-- | Add a constant to the left-hand side of both sides of
-- the inequality.
incrementL :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (c + a <= c + b)
incrementL Lte = Lte

-- | Add a constant to the right-hand side of both sides of
-- the inequality.
incrementR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (a + c <= b + c)
incrementR Lte = Lte

-- | Add a constant to the left-hand side of the right-hand side of
-- the inequality.
weakenL :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (a <= c + b)
weakenL Lte = Lte

-- | Add a constant to the right-hand side of the right-hand side of
-- the inequality.
weakenR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (a <= b + c)
weakenR Lte = Lte

-- | Weaken a strict inequality to a non-strict inequality.
fromStrict :: (a < b) -> (a <= b)
fromStrict Lt = Lte

-- | Zero is less-than-or-equal-to any number.
zero :: 0 <= a
zero = Lte

-- | Use GHC's built-in type-level arithmetic to prove
-- that one number is less-than-or-equal-to another. The type-checker
-- only reduces 'CmpNat' if both arguments are constants.
constant :: forall a b. (IsLte (CmpNat a b) ~ 'True) => (a <= b)
constant = Lte

type family IsLte (o :: Ordering) :: Bool where
  IsLte 'GT = 'False
  IsLte 'LT = 'True
  IsLte 'EQ = 'True
