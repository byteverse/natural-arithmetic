{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

module Arithmetic.Lte
  ( -- * Special Inequalities
    zero
  , reflexive
  , reflexive#
    -- * Substitution
  , substituteL
  , substituteR
    -- * Increment
  , incrementL
  , incrementL#
  , incrementR
  , incrementR#
    -- * Decrement
  , decrementL
  , decrementL#
  , decrementR
  , decrementR#
    -- * Weaken
  , weakenL
  , weakenL#
  , weakenR
  , weakenR#
    -- * Composition
  , transitive
  , transitive#
  , plus
  , plus#
    -- * Convert Strict Inequality
  , fromStrict
  , fromStrict#
  , fromStrictSucc
  , fromStrictSucc#
    -- * Integration with GHC solver
  , constant
    -- * Lift and Unlift
  , lift
  , unlift
  ) where

import Arithmetic.Unsafe (type (<)(Lt),type (:=:)(Eq))
import Arithmetic.Unsafe (type (<=)(Lte))
import Arithmetic.Unsafe (type (<=#)(Lte#),type (<#))
import GHC.TypeNats (CmpNat,type (+))

import qualified GHC.TypeNats as GHC

-- | Replace the left-hand side of a strict inequality
-- with an equal number.
substituteL :: (b :=: c) -> (b <= a) -> (c <= a)
{-# inline substituteL #-}
substituteL Eq Lte = Lte

-- | Replace the right-hand side of a strict inequality
-- with an equal number.
substituteR :: (b :=: c) -> (a <= b) -> (a <= c)
{-# inline substituteR #-}
substituteR Eq Lte = Lte

-- | Add two inequalities.
plus :: (a <= b) -> (c <= d) -> (a + c <= b + d)
{-# inline plus #-}
plus Lte Lte = Lte

plus# :: (a <=# b) -> (c <=# d) -> (a + c <=# b + d)
{-# inline plus# #-}
plus# _ _ = Lte# (# #)

-- | Compose two inequalities using transitivity.
transitive :: (a <= b) -> (b <= c) -> (a <= c)
{-# inline transitive #-}
transitive Lte Lte = Lte

transitive# :: (a <=# b) -> (b <=# c) -> (a <=# c)
{-# inline transitive# #-}
transitive# _ _ = Lte# (# #)

-- | Any number is less-than-or-equal-to itself.
reflexive :: a <= a
{-# inline reflexive #-}
reflexive = Lte

reflexive# :: (# #) -> a <=# a
{-# inline reflexive# #-}
reflexive# _ = Lte# (# #)

-- | Add a constant to the left-hand side of both sides of
-- the inequality.
incrementL :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (c + a <= c + b)
{-# inline incrementL #-}
incrementL Lte = Lte

incrementL# :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <=# b) -> (c + a <=# c + b)
{-# inline incrementL# #-}
incrementL# _ = Lte# (# #)

-- | Add a constant to the right-hand side of both sides of
-- the inequality.
incrementR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (a + c <= b + c)
{-# inline incrementR #-}
incrementR Lte = Lte

incrementR# :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <=# b) -> (a + c <=# b + c)
{-# inline incrementR# #-}
incrementR# _ = Lte# (# #)

-- | Add a constant to the left-hand side of the right-hand side of
-- the inequality.
weakenL :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (a <= c + b)
{-# inline weakenL #-}
weakenL Lte = Lte

weakenL# :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <=# b) -> (a <=# c + b)
{-# inline weakenL# #-}
weakenL# _ = Lte# (# #)

-- | Add a constant to the right-hand side of the right-hand side of
-- the inequality.
weakenR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <= b) -> (a <= b + c)
{-# inline weakenR #-}
weakenR Lte = Lte

weakenR# :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <=# b) -> (a <=# b + c)
{-# inline weakenR# #-}
weakenR# _ = Lte# (# #)

-- | Subtract a constant from the left-hand side of both sides of
-- the inequality. This is the opposite of 'incrementL'.
decrementL :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (c + a <= c + b) -> (a <= b)
{-# inline decrementL #-}
decrementL Lte = Lte

decrementL# :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (c + a <=# c + b) -> (a <=# b)
{-# inline decrementL# #-}
decrementL# _ = Lte# (# #)

-- | Subtract a constant from the right-hand side of both sides of
-- the inequality. This is the opposite of 'incrementR'.
decrementR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a + c <= b + c) -> (a <= b)
{-# inline decrementR #-}
decrementR Lte = Lte

decrementR# :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a + c <=# b + c) -> (a <=# b)
{-# inline decrementR# #-}
decrementR# _ = Lte# (# #)

-- | Weaken a strict inequality to a non-strict inequality.
fromStrict :: (a < b) -> (a <= b)
{-# inline fromStrict #-}
fromStrict Lt = Lte

fromStrict# :: (a <# b) -> (a <=# b)
{-# inline fromStrict# #-}
fromStrict# _ = Lte# (# #)

-- | Weaken a strict inequality to a non-strict inequality, incrementing
-- the right-hand argument by one.
fromStrictSucc :: (a < b) -> (a + 1 <= b)
{-# inline fromStrictSucc #-}
fromStrictSucc Lt = Lte

fromStrictSucc# :: (a <# b) -> (a + 1 <=# b)
{-# inline fromStrictSucc# #-}
fromStrictSucc# _ = Lte# (# #)

-- | Zero is less-than-or-equal-to any number.
zero :: 0 <= a
{-# inline zero #-}
zero = Lte

-- | Use GHC's built-in type-level arithmetic to prove
-- that one number is less-than-or-equal-to another. The type-checker
-- only reduces 'CmpNat' if both arguments are constants.
constant :: forall a b. (IsLte (CmpNat a b) ~ 'True) => (a <= b)
{-# inline constant #-}
constant = Lte

type family IsLte (o :: Ordering) :: Bool where
  IsLte 'GT = 'False
  IsLte 'LT = 'True
  IsLte 'EQ = 'True

unlift :: (a <= b) -> (a <=# b)
unlift _ = Lte# (# #)

lift :: (a <=# b) -> (a <= b)
lift _ = Lte
