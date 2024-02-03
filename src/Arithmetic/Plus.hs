{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Arithmetic.Plus
  ( zeroL
  , zeroR
  , commutative
  , associative
  ) where

import Arithmetic.Unsafe (type (:=:) (Eq))
import GHC.TypeNats (type (+))

-- | Any number plus zero is equal to the original number.
zeroR :: m :=: (m + 0)
zeroR = Eq

-- | Zero plus any number is equal to the original number.
zeroL :: m :=: (0 + m)
zeroL = Eq

-- | Addition is commutative.
commutative :: forall a b. a + b :=: b + a
commutative = Eq

-- | Addition is associative.
associative :: forall a b c. (a + b) + c :=: a + (b + c)
associative = Eq
