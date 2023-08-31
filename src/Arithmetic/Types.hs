{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language ExplicitNamespaces #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}

module Arithmetic.Types
  ( Nat
  , Nat#
  , WithNat(..)
  , Difference(..)
  , Fin(..)
  , Fin#
  , Fin32#
    -- * Maybe Fin
  , MaybeFin#
  , pattern MaybeFinJust#
  , pattern MaybeFinNothing#
    -- * Infix Operators
  , type (<)
  , type (<=)
  , type (<#)
  , type (<=#)
  , type (:=:)
  , type (:=:#)
  ) where

import Arithmetic.Unsafe (Fin#(Fin#),Nat#,Nat(getNat), type (<=))
import Arithmetic.Unsafe (Fin32#)
import Arithmetic.Unsafe (MaybeFin#(..))
import Arithmetic.Unsafe (type (<), type (:=:))
import Arithmetic.Unsafe (type (<#), type (<=#), (:=:#))
import Data.Kind (type Type)
import GHC.TypeNats (type (+))

import qualified GHC.TypeNats as GHC

data WithNat :: (GHC.Nat -> Type) -> Type where
  WithNat ::
       {-# UNPACK #-} !(Nat n)
    -> f n
    -> WithNat f

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

pattern MaybeFinJust# :: Fin# n -> MaybeFin# n
pattern MaybeFinJust# f <- (maybeFinToFin# -> (# | f #)) where
  MaybeFinJust# (Fin# i) = MaybeFin# i

pattern MaybeFinNothing# :: MaybeFin# n
pattern MaybeFinNothing# = MaybeFin# (-1#)

maybeFinToFin# :: MaybeFin# n -> (# (# #) | Fin# n #)
{-# inline maybeFinToFin# #-}
maybeFinToFin# (MaybeFin# i) = case i of
  -1# -> (# (# #) | #)
  _ -> (# | Fin# i #)
