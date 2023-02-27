{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}

module Arithmetic.Nat
  ( -- * Addition
    plus
    -- * Subtraction
  , monus
    -- * Division
  , divide
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
    -- * Convert
  , demote
  , with
  ) where

import Prelude hiding (succ)

import Arithmetic.Types
import Arithmetic.Unsafe ((:=:)(Eq), type (<=)(Lte))
import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt))
import GHC.Exts (Proxy#,proxy#)
import GHC.TypeNats (type (+),Div,KnownNat,natVal')

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

-- | Divide two numbers.
divide :: Nat a -> Nat b -> Nat (Div a b)
{-# inline divide #-}
divide (Nat x) (Nat y) = Nat (div x y)

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
