{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}

module Arithmetic.Types
  ( Nat
  , Fin(..)
  , type (<)
  , type (<=)
  , type (:=:)
  ) where

import Data.Kind (type Type)
import Arithmetic.Unsafe (Nat(getNat), type (<=))
import Arithmetic.Unsafe (type (<), type (:=:))
import qualified GHC.TypeNats as GHC

-- | A finite set of 'n' elements. 'Fin n = { 0 .. n - 1 }'
data Fin :: GHC.Nat -> Type where
  Fin :: forall n m.
    { index :: !(Nat m)
    , proof :: !(m < n)
    } -> Fin n

instance Show (Fin n) where
  showsPrec p (Fin i _) = showString "Fin " . showsPrec p (getNat i)

instance Eq (Fin n) where
  Fin x _ == Fin y _ = getNat x == getNat y

instance Ord (Fin n) where
  Fin x _ `compare` Fin y _ = compare (getNat x) (getNat y)
