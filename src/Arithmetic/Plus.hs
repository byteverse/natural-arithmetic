{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}
{-# language AllowAmbiguousTypes #-}

module Arithmetic.Plus
  ( zeroL
  , zeroR
  , commutative
  , associative
  ) where

import Arithmetic.Unsafe (type (:=:)(Equal))
import GHC.TypeNats (type (+))

-- | Any number plus zero is equal to the original number.
zeroR :: m :=: (m + 0)
zeroR = Equal

-- | Zero plus any number is equal to the original number.
zeroL :: m :=: (m + 0)
zeroL = Equal

-- | Addition is commutative.
commutative :: forall a b. a + b :=: b + a
commutative = Equal

-- | Addition is associative.
associative :: forall a b c. (a + b) + c :=: a + (b + c)
associative = Equal
