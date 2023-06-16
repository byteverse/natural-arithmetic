{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RoleAnnotations #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module Arithmetic.Unsafe
  ( Nat(..)
  , Nat#(..)
  , Fin#(..)
  , Fin32#(..)
  , type (<#)(Lt#)
  , type (<=#)(Lte#)
  , type (<)(Lt)
  , type (<=)(Lte)
  , type (:=:)(Eq)
  , type (:=:#)(Eq#)
  ) where

import Prelude hiding ((>=),(<=))

import Control.Category (Category)
import Data.Kind (Type)
import GHC.Exts (Int#,Int32#,TYPE,RuntimeRep(IntRep,Int32Rep,TupleRep))

import qualified Control.Category
import qualified GHC.TypeNats as GHC

-- Do not import this module unless you enjoy pain.
-- Using this library to implement length-indexed arrays
-- or sized builders does not require importing this
-- module to get the value out of the Nat data constructor.
-- Use Arithmetic.Nat.demote for this purpose.

infix 4 <
infix 4 <=
infix 4 <#
infix 4 <=#
infix 4 :=:
infix 4 :=:#

-- | A value-level representation of a natural number @n@.
newtype Nat (n :: GHC.Nat) = Nat { getNat :: Int }
type role Nat nominal

deriving newtype instance Show (Nat n)

-- | Unboxed variant of Nat.
newtype Nat# :: GHC.Nat -> TYPE 'IntRep where
  Nat# :: Int# -> Nat# n
type role Nat# nominal

-- | Finite numbers without the overhead of carrying around a proof.
newtype Fin# :: GHC.Nat -> TYPE 'IntRep where
  Fin# :: Int# -> Fin# n
type role Fin# nominal

-- | Variant of 'Fin#' that only allows 32-bit integers.
newtype Fin32# :: GHC.Nat -> TYPE 'Int32Rep where
  Fin32# :: Int32# -> Fin32# n
type role Fin32# nominal

-- | Proof that the first argument is strictly less than the
-- second argument.
data (<) :: GHC.Nat -> GHC.Nat -> Type where
  Lt :: a < b

newtype (<#) :: GHC.Nat -> GHC.Nat -> TYPE ('TupleRep '[]) where
  Lt# :: (# #) -> a <# b

-- | Proof that the first argument is less than or equal to the
-- second argument.
data (<=) :: GHC.Nat -> GHC.Nat -> Type where
  Lte :: a <= b

newtype (<=#) :: GHC.Nat -> GHC.Nat -> TYPE ('TupleRep '[]) where
  Lte# :: (# #) -> a <=# b

-- | Proof that the first argument is equal to the second argument.
data (:=:) :: GHC.Nat -> GHC.Nat -> Type where
  Eq :: a :=: b

newtype (:=:#) :: GHC.Nat -> GHC.Nat -> TYPE ('TupleRep '[]) where
  Eq# :: (# #) -> a :=:# b

instance Category (<=) where
  id = Lte
  Lte . Lte = Lte

instance Category (:=:) where
  id = Eq
  Eq . Eq = Eq
