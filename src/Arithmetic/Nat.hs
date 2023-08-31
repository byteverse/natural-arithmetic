{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}

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
  , succ#
    -- * Compare
  , testEqual
  , testEqual#
  , testLessThan
  , testLessThan#
  , testLessThanEqual
  , testZero
  , testZero#
  , (=?)
  , (<?)
  , (<?#)
  , (<=?)
    -- * Constants
  , zero
  , one
  , two
  , three
  , constant
  , constant#
    -- * Unboxed Constants
  , zero#
  , one#
    -- * Unboxed Pattern Synonyms
  , pattern N0#
  , pattern N1#
  , pattern N2#
  , pattern N3#
  , pattern N4#
  , pattern N8#
  , pattern N16#
  , pattern N32#
  , pattern N64#
  , pattern N128#
  , pattern N256#
  , pattern N512#
  , pattern N1024#
  , pattern N2048#
  , pattern N4096#
    -- * Convert
  , demote
  , demote#
  , unlift
  , lift
  , with
  , with#
  ) where

import Prelude hiding (succ)

import Arithmetic.Types
import Arithmetic.Unsafe ((:=:)(Eq), type (<=)(Lte), (:=:#)(Eq#))
import Arithmetic.Unsafe (Nat(Nat),Nat#(Nat#),type (<)(Lt),type (<#)(Lt#))
import GHC.Exts (Proxy#,proxy#,(+#),(<#),Int#,(==#))
import GHC.TypeNats (type (+),type (-),Div,KnownNat,natVal')
import GHC.Int (Int(I#))
import Data.Maybe.Void (MaybeVoid#, pattern JustVoid#, pattern NothingVoid#)
import Data.Either.Void (EitherVoid#, pattern LeftVoid#, pattern RightVoid#)

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

(<?#) :: Nat# a -> Nat# b -> MaybeVoid# (a <# b)
{-# inline (<?#) #-}
(<?#) = testLessThan#

-- | Is the first argument strictly less than the second
-- argument?
testLessThan :: Nat a -> Nat b -> Maybe (a < b)
{-# inline testLessThan #-}
testLessThan (Nat x) (Nat y) = if x < y
  then Just Lt
  else Nothing

testLessThan# :: Nat# a -> Nat# b -> MaybeVoid# (a <# b)
{-# inline testLessThan# #-}
testLessThan# (Nat# x) (Nat# y) = case x <# y of
  0# -> NothingVoid#
  _ -> JustVoid# (Lt# (# #))

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

testEqual# :: Nat# a -> Nat# b -> MaybeVoid# (a :=:# b)
{-# inline testEqual# #-}
testEqual# (Nat# x) (Nat# y) = case x ==# y of
  0# -> NothingVoid#
  _ -> JustVoid# (Eq# (# #))

-- | Is zero equal to this number or less than it?
testZero :: Nat a -> Either (0 :=: a) (0 < a)
{-# inline testZero #-}
testZero (Nat x) = case x of
  0 -> Left Eq
  _ -> Right Lt

testZero# :: Nat# a -> EitherVoid# (0 :=:# a) (0 <# a)
testZero# (Nat# x) = case x of
  0# -> LeftVoid# (Eq# (# #))
  _ -> RightVoid# (Lt# (# #))

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

-- | Unlifted variant of 'succ'.
succ# :: Nat# a -> Nat# (a + 1)
{-# inline succ# #-}
succ# n = plus# n (one# (# #))

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

constant# :: forall n. KnownNat n => (# #) -> Nat# n
{-# inline constant# #-}
constant# _ = case fromIntegral (natVal' (proxy# :: Proxy# n)) of
  I# i -> Nat# i

-- | The number zero. Unboxed.
zero# :: (# #) -> Nat# 0
zero# _ = Nat# 0#

-- | The number one. Unboxed.
one# :: (# #) -> Nat# 1
one# _ = Nat# 1#

-- | Extract the 'Int' from a 'Nat'. This is intended to be used
-- at a boundary where a safe interface meets the unsafe primitives
-- on top of which it is built.
demote :: Nat n -> Int
{-# inline demote #-}
demote (Nat n) = n

demote# :: Nat# n -> Int#
{-# inline demote# #-}
demote# (Nat# n) = n

-- | Run a computation on a witness of a type-level number. The
-- argument 'Int' must be greater than or equal to zero. This is
-- not checked. Failure to upload this invariant will lead to a
-- segfault.
with :: Int -> (forall n. Nat n -> a) -> a
{-# inline with #-}
with i f = f (Nat i)

with# :: Int# -> (forall n. Nat# n -> a) -> a
{-# inline with# #-}
with# i f = f (Nat# i)

unlift :: Nat n -> Nat# n
{-# inline unlift #-}
unlift (Nat (I# i)) = Nat# i

lift :: Nat# n -> Nat n
{-# inline lift #-}
lift (Nat# i) = Nat (I# i)

pattern N0# :: Nat# 0
pattern N0# = Nat# 0#

pattern N1# :: Nat# 1
pattern N1# = Nat# 1#

pattern N2# :: Nat# 2
pattern N2# = Nat# 2#

pattern N3# :: Nat# 3
pattern N3# = Nat# 3#

pattern N4# :: Nat# 4
pattern N4# = Nat# 4#

pattern N8# :: Nat# 8
pattern N8# = Nat# 8#

pattern N16# :: Nat# 16
pattern N16# = Nat# 16#

pattern N32# :: Nat# 32
pattern N32# = Nat# 32#

pattern N64# :: Nat# 64
pattern N64# = Nat# 64#

pattern N128# :: Nat# 128
pattern N128# = Nat# 128#

pattern N256# :: Nat# 256
pattern N256# = Nat# 256#

pattern N512# :: Nat# 512
pattern N512# = Nat# 512#

pattern N1024# :: Nat# 1024
pattern N1024# = Nat# 1024#

pattern N2048# :: Nat# 2048
pattern N2048# = Nat# 2048#

pattern N4096# :: Nat# 4096
pattern N4096# = Nat# 4096#
