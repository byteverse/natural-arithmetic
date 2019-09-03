{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}
{-# language TypeOperators #-}
{-# language TypeApplications #-}
{-# language ExplicitNamespaces #-}
module Arithmetic.Fin
  ( -- * Construction
    zero
  , last
  , ascending
  , descending
  , slice
    -- * Modification
  , raise
  , raiseN
  , weaken
  , weakenN
    -- * Absurdities
  , finZeroAbsurd
    -- * Demote
  , demote
  ) where

import Prelude hiding (last)
import GHC.TypeNats (type (+))
import Arithmetic.Unsafe (Nat(..), type (<)(Lt))
import Arithmetic.Types (Fin(..))
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte

-- | The smallest member of a finite set
zero :: Nat n -> 0 < n -> Fin n
zero _ !pf = Fin Nat.zero pf

-- | The largest member of a finite set
last :: Nat n -> 0 < n -> Fin n
last n !pf = Fin (Nat (getNat n - 1)) pf

-- | Raise the index by one and weaken the bound by one.
raise :: Fin n -> Fin (n + 1)
raise = raiseN Nat.one

-- | Raise the index by 'm' and weaken the bound by 'm'.
-- 'raise Nat.one == raise'
raiseN :: forall n m. Nat m -> Fin n -> Fin (n + m)
raiseN m (Fin i pf) = Fin (Nat.plus i m) (Lt.plus pf Lte.reflexive)

-- | Weaken the bound by one. This does not change the index.
weaken :: forall n m. Fin n -> Fin (n + m)
weaken (Fin i pf) = Fin i (Lt.plus pf Lte.zero)

-- | Weaken the bound by 'm'. This does not change the index.
weakenN :: forall n m. Fin n -> Fin (n + m)
weakenN (Fin i pf) = Fin i (Lt.plus pf Lte.zero)

-- | A finite set of no values is impossible
finZeroAbsurd :: Fin 0 -> void
finZeroAbsurd (Fin _ pf) = Lt.lessThanZeroAbsurd pf

-- | Generate all values of a finite set in ascending order
-- >>> ascending (Nat.constant @3) (Lt.constant @3)
-- [0, 1, 2]
ascending :: forall n. Nat n -> 0 < n -> [Fin n]
ascending n !_ = go 0
  where
    go :: Int -> [Fin n]
    go !m
      | getNat n == m = []
      | otherwise = Fin (Nat m) Lt : go (m + 1)

-- | Generate all values of a finite set in descending order
-- >>> descending (Nat.constant @3) (Lt.constant @3)
-- [2, 1, 0]
descending :: forall n. Nat n -> 0 < n -> [Fin n]
descending n !_ = go (getNat n - 1)
  where
    go :: Int -> [Fin n]
    go !m
      | m < 0 = []
      | otherwise = Fin (Nat m) Lt : go (m - 1)

-- | Generate 'len' values starting from 'offset'
-- >>> slice (Nat.constant @2) (Nat.constant @3) (Lt.constant @6)
-- [2, 3, 4]
slice :: forall n offset len. Nat offset -> Nat len -> offset + len < n -> [Fin n]
slice offset len !_ = go 0
  where
    go :: Int -> [Fin n]
    go !m
      | m == getNat len = []
      | otherwise = Fin (Nat (m + getNat offset)) Lt : go (m + 1)

-- | Extract the 'Int' from a 'Fin n'. This is intended to be used
-- at a boundary where a safe interface meets the unsafe primitives
-- on top of which it is built.
demote :: Fin n -> Int
demote (Fin i _) = Nat.demote i
