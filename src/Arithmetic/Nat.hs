{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

module Arithmetic.Nat
  ( -- * Addition
    plus
  , plus#
    -- * Subtraction
  , monus
    -- * Division
  , divide
  , divideRoundingUp
    -- * Multiplication
  , times
    -- * Successor
  , succ
    -- * Compare
  , testEqual
  , testLessThan
  , testLessThanEqual
  , testZero
  , (=?)
  , (<?)
  , (<=?)
    -- * Constants
  , zero
  , one
  , two
  , three
  , constant
    -- * Unboxed Constants
  , zero#
    -- * Convert
  , demote
  , unlift
  , lift
  , with
  ) where

import Prelude hiding (succ)

import Arithmetic.Types
import Arithmetic.Unsafe ((:=:)(Eq), type (<=)(Lte))
import Arithmetic.Unsafe (Nat(Nat),Nat#(Nat#),type (<)(Lt))
import GHC.Exts (Proxy#,proxy#,(+#))
import GHC.TypeNats (type (+),type (-),Div,KnownNat,natVal')
import GHC.Int (Int(I#))

import qualified GHC.TypeNats as GHC

-- | Infix synonym of 'testLessThan'.
(<?) :: Nat a -> Nat b -> Maybe (a < b)
{-# inline (<?) #-}
(<?) = testLessThan

-- | Infix synonym of 'testLessThanEqual'.
(<=?) :: Nat a -> Nat b -> Maybe (a <= b)
{-# inline (<=?) #-}
(<=?) = testLessThanEqual

-- | Infix synonym of 'testEqual'.
(=?) :: Nat a -> Nat b -> Maybe (a :=: b)
{-# inline (=?) #-}
(=?) = testEqual

-- | Is the first argument strictly less than the second
-- argument?
testLessThan :: Nat a -> Nat b -> Maybe (a < b)
{-# inline testLessThan #-}
testLessThan (Nat x) (Nat y) = if x < y
  then Just Lt
  else Nothing

-- | Is the first argument less-than-or-equal-to the second
-- argument?
testLessThanEqual :: Nat a -> Nat b -> Maybe (a <= b)
{-# inline testLessThanEqual #-}
testLessThanEqual (Nat x) (Nat y) = if x <= y
  then Just Lte
  else Nothing

-- | Are the two arguments equal to one another?
testEqual :: Nat a -> Nat b -> Maybe (a :=: b)
{-# inline testEqual #-}
testEqual (Nat x) (Nat y) = if x == y
  then Just Eq
  else Nothing

-- | Is zero equal to this number or less than it?
testZero :: Nat a -> Either (0 :=: a) (0 < a)
{-# inline testZero #-}
testZero (Nat x) = case x of
  0 -> Left Eq
  _ -> Right Lt

-- | Add two numbers.
plus :: Nat a -> Nat b -> Nat (a + b)
{-# inline plus #-}
plus (Nat x) (Nat y) = Nat (x + y)

-- | Variant of 'plus' for unboxed nats.
plus# :: Nat# a -> Nat# b -> Nat# (a + b)
{-# inline plus# #-}
plus# (Nat# x) (Nat# y) = Nat# (x +# y)

-- | Divide two numbers. Rounds down (towards zero)
divide :: Nat a -> Nat b -> Nat (Div a b)
{-# inline divide #-}
divide (Nat x) (Nat y) = Nat (div x y)

-- | Divide two numbers. Rounds up (away from zero)
divideRoundingUp :: Nat a -> Nat b -> Nat (Div (a - 1) b + 1)
{-# inline divideRoundingUp #-}
divideRoundingUp (Nat x) (Nat y) =
  -- Implementation note. We must use div so that when x=0,
  -- the result is (-1) and not 0. Then when we add 1, we get 0.
  Nat (1 + (div (x - 1) y))

-- | Multiply two numbers.
times :: Nat a -> Nat b -> Nat (a GHC.* b)
{-# inline times #-}
times (Nat x) (Nat y) = Nat (x * y)

-- | The successor of a number.
succ :: Nat a -> Nat (a + 1)
{-# inline succ #-}
succ n = plus n one

-- | Subtract the second argument from the first argument.
monus :: Nat a -> Nat b -> Maybe (Difference a b)
{-# inline monus #-}
monus (Nat a) (Nat b) = let c = a - b in if c >= 0
  then Just (Difference (Nat c) Eq)
  else Nothing

-- | The number zero.
zero :: Nat 0
{-# inline zero #-}
zero = Nat 0

-- | The number one.
one :: Nat 1
{-# inline one #-}
one = Nat 1

-- | The number two.
two :: Nat 2
{-# inline two #-}
two = Nat 2

-- | The number three.
three :: Nat 3
{-# inline three #-}
three = Nat 3

-- | Use GHC's built-in type-level arithmetic to create a witness
-- of a type-level number. This only reduces if the number is a
-- constant.
constant :: forall n. KnownNat n => Nat n
{-# inline constant #-}
constant = Nat (fromIntegral (natVal' (proxy# :: Proxy# n)))

-- | The number zero. Unboxed.
zero# :: (# #) -> Nat# 0
zero# _ = Nat# 0#

-- | Extract the 'Int' from a 'Nat'. This is intended to be used
-- at a boundary where a safe interface meets the unsafe primitives
-- on top of which it is built.
demote :: Nat n -> Int
{-# inline demote #-}
demote (Nat n) = n

-- | Run a computation on a witness of a type-level number. The
-- argument 'Int' must be greater than or equal to zero. This is
-- not checked. Failure to upload this invariant will lead to a
-- segfault.
with :: Int -> (forall n. Nat n -> a) -> a
{-# inline with #-}
with i f = f (Nat i)

unlift :: Nat n -> Nat# n
{-# inline unlift #-}
unlift (Nat (I# i)) = Nat# i

lift :: Nat# n -> Nat n
{-# inline lift #-}
lift (Nat# i) = Nat (I# i)
