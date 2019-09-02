{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Arithmetic.Lte
  ( transitive
  , plus
  , zero
  , substituteL
  , substituteR
  , incrementL
  , incrementR
  , reflexive
  , fromStrict
    -- * Integration with GHC solver
  , constant
  ) where

import Arithmetic.Unsafe (type (<)(Lt),type (:=:)(Equal))
import Arithmetic.Unsafe (type (<=)(Lte))
import GHC.TypeNats (CmpNat,type (+))
import qualified GHC.TypeNats as GHC

-- | Replace the right-hand side of a strict inequality
-- with an equal number.
substituteL :: (b :=: c) -> (a < b) -> (a < c)
substituteL Equal Lt = Lt

-- | Replace the right-hand side of a strict inequality
-- with an equal number.
substituteR :: (b :=: c) -> (a < b) -> (a < c)
substituteR Equal Lt = Lt

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

-- | Weaken a strict inequality.
fromStrict :: (a < b) -> (a <= b)
fromStrict Lt = Lte

zero :: 0 <= a
zero = Lte

constant :: forall a b. (IsLte (CmpNat a b) ~ 'True) => (a <= b)
constant = Lte

type family IsLte (o :: Ordering) :: Bool where
  IsLte 'GT = 'False
  IsLte 'LT = 'True
  IsLte 'EQ = 'True
