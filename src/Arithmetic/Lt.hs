{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}
{-# language TypeFamilies #-}

module Arithmetic.Lt
  ( -- * Special Inequalities
    zero
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
  , plus
  , transitive
    -- * Absurdities
  , absurd
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

-- | Add a strict inequality to a nonstrict inequality.
plus :: (a < b) -> (c <= d) -> (a + c < b + d)
plus Lt Lte = Lt

-- | Add a constant to the left-hand side of both sides of
-- the strict inequality.
incrementL :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) -> (c + a < c + b)
incrementL Lt = Lt

-- | Add a constant to the right-hand side of both sides of
-- the strict inequality.
incrementR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) -> (a + c < b + c)
incrementR Lt = Lt

-- | Add a constant to the left-hand side of the right-hand side of
-- the strict inequality.
weakenL :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) -> (a < c + b)
weakenL Lt = Lt

-- | Add a constant to the right-hand side of the right-hand side of
-- the strict inequality.
weakenR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) -> (a < b + c)
weakenR Lt = Lt

-- | Compose two strict inequalities using transitivity.
transitive :: (a < b) -> (b < c) -> (a < c)
transitive Lt Lt = Lt

-- | Zero is less than one.
zero :: 0 < 1
zero = Lt

-- | Nothing is less than zero.
absurd :: n < 0 -> void
absurd Lt = error "Arithmetic.Nat.absurd: n < 0"

-- | Use GHC's built-in type-level arithmetic to prove
-- that one number is less than another. The type-checker
-- only reduces 'CmpNat' if both arguments are constants.
constant :: forall a b. (CmpNat a b ~ 'LT) => (a < b)
constant = Lt

