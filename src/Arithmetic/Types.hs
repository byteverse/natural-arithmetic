{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}

module Arithmetic.Types
  ( Nat
  , WithNat(..)
  , Difference(..)
  , Fin(..)
  , type (<)
  , type (<=)
  , type (:=:)
  ) where

import Arithmetic.Unsafe (Nat(getNat), type (<=))
import Arithmetic.Unsafe (type (<), type (:=:))
import Data.Kind (type Type)
import GHC.TypeNats (type (+))

import qualified GHC.TypeNats as GHC

data WithNat :: (GHC.Nat -> Type) -> Type where
  WithNat :: Nat n -> f n -> WithNat f

-- | A finite set of 'n' elements. 'Fin n = { 0 .. n - 1 }'
data Fin :: GHC.Nat -> Type where
  Fin :: forall m n.
    { index :: !(Nat m)
    , proof :: !(m < n)
    } -> Fin n

-- | Proof that the first argument can be expressed as the
-- sum of the second argument and some other natural number.
data Difference :: GHC.Nat -> GHC.Nat -> Type where
  -- It is safe for users of this library to use this data constructor
  -- freely. However, note that the interesting Difference values come
  -- from Arithmetic.Nat.monus, which is a primitive.
  Difference :: forall a b c. Nat c -> (c + b :=: a) -> Difference a b

instance Show (Fin n) where
  showsPrec p (Fin i _) = showString "Fin " . showsPrec p (getNat i)

instance Eq (Fin n) where
  Fin x _ == Fin y _ = getNat x == getNat y

instance Ord (Fin n) where
  Fin x _ `compare` Fin y _ = compare (getNat x) (getNat y)
