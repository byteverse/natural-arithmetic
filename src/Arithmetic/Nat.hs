{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}

module Arithmetic.Nat
  ( -- * Addition
    plus
    -- * Subtraction
  , monus
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
  , constant
    -- * Demote
  , demote
  ) where

import Prelude hiding (succ)

import Arithmetic.Types
import Arithmetic.Unsafe ((:=:)(Eq), type (<=)(Lte))
import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt))
import GHC.Exts (Proxy#,proxy#)
import GHC.TypeNats (type (+),KnownNat,natVal')

-- | Infix synonym of 'testLessThan'.
(<?) :: Nat a -> Nat b -> Maybe (a < b)
(<?) = testLessThan

-- | Infix synonym of 'testLessThanEqual'.
(<=?) :: Nat a -> Nat b -> Maybe (a <= b)
(<=?) = testLessThanEqual

-- | Infix synonym of 'testEqual'.
(=?) :: Nat a -> Nat b -> Maybe (a :=: b)
(=?) = testEqual

-- | Is the first argument strictly less than the second
-- argument?
testLessThan :: Nat a -> Nat b -> Maybe (a < b)
testLessThan (Nat x) (Nat y) = if x < y
  then Just Lt
  else Nothing

-- | Is the first argument less-than-or-equal-to the second
-- argument?
testLessThanEqual :: Nat a -> Nat b -> Maybe (a <= b)
testLessThanEqual (Nat x) (Nat y) = if x <= y
  then Just Lte
  else Nothing

-- | Are the two arguments equal to one another?
testEqual :: Nat a -> Nat b -> Maybe (a :=: b)
testEqual (Nat x) (Nat y) = if x == y
  then Just Eq
  else Nothing

-- | Is zero equal to this number or less than it?
testZero :: Nat a -> Either (0 :=: a) (0 < a)
testZero (Nat x) = case x of
  0 -> Left Eq
  _ -> Right Lt

-- | Add two numbers.
plus :: Nat a -> Nat b -> Nat (a + b)
plus (Nat x) (Nat y) = Nat (x + y)

-- | The successor of a number.
succ :: Nat a -> Nat (a + 1)
succ n = plus n one

-- | Subtract the second argument from the first argument.
monus :: Nat a -> Nat b -> Maybe (Difference a b)
{-# inline monus #-}
monus (Nat a) (Nat b) = let c = a - b in if c >= 0
  then Just (Difference (Nat c) Eq)
  else Nothing

-- | The number zero.
zero :: Nat 0
zero = Nat 0

-- | The number one.
one :: Nat 1
one = Nat 1

-- | Use GHC's built-in type-level arithmetic to create a witness
-- of a type-level number. This only reduces if the number is a
-- constant.
constant :: forall n. KnownNat n => Nat n
constant = Nat (fromIntegral (natVal' (proxy# :: Proxy# n)))

-- | Extract the 'Int' from a 'Nat'. This is intended to be used
-- at a boundary where a safe interface meets the unsafe primitives
-- on top of which it is built.
demote :: Nat n -> Int
demote (Nat n) = n
