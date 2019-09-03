{-# language RoleAnnotations #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language TypeOperators #-}
{-# language ExplicitNamespaces #-}

module Arithmetic.Unsafe
  ( Nat(..)
  , type (<)(Lt)
  , type (<=)(Lte)
  , type (:=:)(Equal)
  ) where

import Prelude hiding ((>=),(<=))
import Data.Kind (Type)
import Control.Category (Category)
import qualified Control.Category
import qualified GHC.TypeNats as GHC

-- Do not import this module unless you enjoy pain.
-- Using this library to implement length-indexed arrays
-- or sized builders does not require importing this
-- module to get the value out of the Nat data constructor.
-- Use Arithmetic.Nat.

infix 4 <
infix 4 <=
infix 4 :=:

-- | A value-level representation of a natural number @n@.
newtype Nat (n :: GHC.Nat) = Nat { getNat :: Int }
type role Nat nominal

data (<) :: GHC.Nat -> GHC.Nat -> Type where
  Lt :: a < b

data (<=) :: GHC.Nat -> GHC.Nat -> Type where
  Lte :: a <= b

data (:=:) :: GHC.Nat -> GHC.Nat -> Type where
  Equal :: a :=: b

instance Category (<=) where
  id = Lte
  Lte . Lte = Lte

instance Category (:=:) where
  id = Equal
  Equal . Equal = Equal
