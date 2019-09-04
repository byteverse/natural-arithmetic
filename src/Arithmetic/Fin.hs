{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}
{-# language TypeOperators #-}
{-# language TypeApplications #-}
{-# language ExplicitNamespaces #-}
module Arithmetic.Fin
  ( -- * Modification
    incrementL
  , incrementR
  , weakenL
  , weakenR
    -- * Traverse
  , ascend
  , ascendM
  , ascendM_
  , ascending
  , descending
  , ascendingSlice
    -- * Absurdities
  , absurd
    -- * Demote
  , demote
  ) where

import Prelude hiding (last)
import GHC.TypeNats (type (+))
import Arithmetic.Nat ((<?))
import Arithmetic.Types (Fin(..),Difference(..),Nat,type (<), type (<=), type (:=:))
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Plus as Plus

-- | Raise the index by @m@ and weaken the bound by @m@, adding
-- @m@ to the right-hand side of @n@.
incrementR :: forall n m. Nat m -> Fin n -> Fin (n + m)
incrementR m (Fin i pf) = Fin (Nat.plus i m) (Lt.incrementR @m pf)

-- | Raise the index by @m@ and weaken the bound by @m@, adding
-- @m@ to the left-hand side of @n@.
incrementL :: forall n m. Nat m -> Fin n -> Fin (m + n)
incrementL m (Fin i pf) = Fin (Nat.plus m i) (Lt.incrementL @m pf)

-- | Weaken the bound by one. This does not change the index.
weakenL :: forall n m. Fin n -> Fin (m + n)
weakenL (Fin i pf) = Fin i
  ( Lt.substituteR
    (Plus.commutative @n @m)
    (Lt.plus pf (Lte.zero @m))
  )

-- side of @n@. This does not change the index.
weakenR :: forall n m. Fin n -> Fin (n + m)
weakenR (Fin i pf) = Fin i (Lt.plus pf Lte.zero)

-- | A finite set of no values is impossible.
absurd :: Fin 0 -> void
absurd (Fin _ pf) = Lt.absurd pf

-- | Strict fold over the numbers bounded by @n@ in ascending
-- order. For convenince, this differs from @foldl'@ in the
-- order of the parameters differs from @foldl@. Roughly:
--
-- > ascend 4 z f = f 3 (f 2 (f 1 (f 0 z)))
ascend :: forall a n.
     Nat n -- ^ Upper bound
  -> a -- ^ Initial accumulator
  -> (Fin n -> a -> a) -- ^ Update accumulator
  -> a
{-# inline ascend #-}
ascend !n !b0 f = go Nat.zero b0
  where
  go :: Nat m -> a -> a
  go !m !b = case m <? n of
    Nothing -> b
    Just lt -> go (Nat.succ m) (f (Fin m lt) b)

-- | Strict monadic left fold over the numbers bounded by @n@
-- in ascending order. Roughly:
--
-- > ascendM 4 z f =
-- >   f 0 z0 >>= \z1 ->
-- >   f 1 z1 >>= \z2 ->
-- >   f 2 z2 >>= \z3 ->
-- >   f 3 z3
ascendM :: forall m a n. Monad m
  => Nat n -- ^ Upper bound
  -> a -- ^ Initial accumulator
  -> (Fin n -> a -> m a) -- ^ Update accumulator
  -> m a
{-# inline ascendM #-}
ascendM !n !b0 f = go Nat.zero b0
  where
  go :: Nat p -> a -> m a
  go !m !b = case m <? n of
    Nothing -> pure b
    Just lt -> go (Nat.succ m) =<< f (Fin m lt) b

-- | Monadic traversal of the numbers bounded by @n@
-- in ascending order.
--
-- > ascendM_ 4 f = f 0 *> f 1 *> f 2 *> f 3
ascendM_ :: forall m a n. Applicative m
  => Nat n -- ^ Upper bound
  -> (Fin n -> m a) -- ^ Effectful interpretion
  -> m ()
{-# inline ascendM_ #-}
ascendM_ !n f = go Nat.zero
  where
  go :: Nat p -> m ()
  go !m = case m <? n of
    Nothing -> pure ()
    Just lt -> f (Fin m lt) *> go (Nat.succ m)

-- | Generate all values of a finite set in ascending order.
--
-- >>> ascending (Nat.constant @3)
-- [0, 1, 2]
ascending :: forall n. Nat n -> [Fin n]
ascending !n = go Nat.zero
  where
  go :: Nat m -> [Fin n]
  go !m = case m <? n of
    Nothing -> []
    Just lt -> Fin m lt : go (Nat.succ m)

-- | Generate all values of a finite set in descending order.
--
-- >>> descending (Nat.constant @3)
-- [2, 1, 0]
descending :: forall n. Nat n -> [Fin n]
descending n = go n Lte.reflexive
  where
    go :: forall m. Nat m -> (m <= n) -> [Fin n]
    go !m !lt = case Nat.monus m Nat.one of
      Nothing -> []
      Just (Difference mpred eq) -> go2 lt mpred eq
    go2 :: forall m c. (m <= n) -> Nat c -> (c + 1 :=: m) -> [Fin n]
    go2 !lt !c !eq = 
        let ceeLtEm :: c < m
            ceeLtEm = id
              $ Lt.substituteR eq
              $ Lt.substituteL Plus.zeroL
              $ Lt.incrementL @c Lt.zero
         in Fin c (Lt.transitiveNonstrictR ceeLtEm lt) : go c
              (Lte.transitive (Lte.substituteR eq (Lte.weakenR @1 (Lte.reflexive @c))) lt)

-- | Generate 'len' values starting from 'off'.
--
-- >>> slice (Nat.constant @2) (Nat.constant @3) (Lt.constant @6)
-- [2, 3, 4]
ascendingSlice :: forall n off len.
     Nat off
  -> Nat len
  -> (off + len < n)
  -> [Fin n]
ascendingSlice off len !offPlusLenLtEn = go Nat.zero
  where
    go :: Nat m -> [Fin n]
    go !m = case m <? len of
      Nothing -> []
      Just emLtLen ->
        let !offPlusEmLtOffPlusLen = Lt.incrementL @off emLtLen
            !offPlusEmLtEn = Lt.transitive offPlusEmLtOffPlusLen offPlusLenLtEn
         in Fin (Nat.plus off m) offPlusEmLtEn : go (Nat.succ m)

-- | Extract the 'Int' from a 'Fin n'. This is intended to be used
-- at a boundary where a safe interface meets the unsafe primitives
-- on top of which it is built.
demote :: Fin n -> Int
demote (Fin i _) = Nat.demote i
