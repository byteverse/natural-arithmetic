{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
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
  , descendM
  , descendM_
  , ascending
  , descending
  , ascendingSlice
  , descendingSlice
    -- * Absurdities
  , absurd
    -- * Demote
  , demote
  ) where

import Prelude hiding (last)

import Arithmetic.Nat ((<?))
import Arithmetic.Types (Fin(..),Difference(..),Nat,type (<), type (<=), type (:=:))
import GHC.TypeNats (type (+))

import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Equal as Eq
import qualified Arithmetic.Nat as Nat
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

descendLemma :: forall a b c. a + 1 :=: b -> b <= c -> a < c
{-# inline descendLemma #-}
descendLemma !aPlusOneEqB !bLteC = id
  $ Lt.transitiveNonstrictR
      (Lt.substituteR (Plus.commutative @1 @a)
      (Lt.plus Lt.zero Lte.reflexive))
  $ Lte.substituteL (Eq.symmetric aPlusOneEqB) bLteC

-- | Strict monadic left fold over the numbers bounded by @n@
-- in descending order. Roughly:
--
-- > descendM 4 z f =
-- >   f 3 z0 >>= \z1 ->
-- >   f 2 z1 >>= \z2 ->
-- >   f 1 z2 >>= \z3 ->
-- >   f 0 z3
descendM :: forall m a n. Monad m
  => Nat n
  -> a
  -> (Fin n -> a -> m a)
  -> m a
{-# inline descendM #-}
descendM !n !b0 f = go n Lte.reflexive b0
  where
    go :: Nat p -> p <= n -> a -> m a
    go !m pLteEn !b = case Nat.monus m Nat.one of
      Nothing -> pure b
      Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
        let !cLtEn = descendLemma cPlusOneEqP pLteEn
        in go mpred (Lte.fromStrict cLtEn) =<< f (Fin mpred cLtEn) b

-- | Monadic traversal of the numbers bounded by @n@
-- in descending order.
--
-- > descendM_ 4 f = f 3 *> f 2 *> f 1 *> f 0
descendM_ :: forall m a n. Applicative m
  => Nat n -- ^ Upper bound
  -> (Fin n -> m a) -- ^ Effectful interpretion
  -> m ()
{-# inline descendM_ #-}
descendM_ !n f = go n Lte.reflexive
  where
  go :: Nat p -> p <= n -> m ()
  go !m !pLteEn = case Nat.monus m Nat.one of
    Nothing -> pure ()
    Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
      let !cLtEn = descendLemma cPlusOneEqP pLteEn
      in f (Fin mpred cLtEn) *> go mpred (Lte.fromStrict cLtEn)

-- | Generate all values of a finite set in ascending order.
--
-- >>> ascending (Nat.constant @3)
-- [Fin 0,Fin 1,Fin 2]
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
-- [Fin 2,Fin 1,Fin 0]
descending :: forall n. Nat n -> [Fin n]
descending !n = go n Lte.reflexive
  where
    go :: Nat p -> (p <= n) -> [Fin n]
    go !m !pLteEn = case Nat.monus m Nat.one of
      Nothing -> []
      Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
        let !cLtEn = descendLemma cPlusOneEqP pLteEn
        in Fin mpred cLtEn : go mpred (Lte.fromStrict cLtEn)

-- | Generate 'len' values starting from 'off' in ascending order.
--
-- >>> ascendingSlice (Nat.constant @2) (Nat.constant @3) (Lte.constant @_ @6)
-- [Fin 2,Fin 3,Fin 4]
ascendingSlice
  :: forall n off len
  .  Nat off
  -> Nat len
  -> off + len <= n
  -> [Fin n]
{-# inline ascendingSlice #-}
ascendingSlice off len !offPlusLenLteEn = go Nat.zero
  where
    go :: Nat m -> [Fin n]
    go !m = case m <? len of
      Nothing -> []
      Just emLtLen ->
        let !offPlusEmLtOffPlusLen = Lt.incrementL @off emLtLen
            !offPlusEmLtEn = Lt.transitiveNonstrictR offPlusEmLtOffPlusLen offPlusLenLteEn
         in Fin (Nat.plus off m) offPlusEmLtEn : go (Nat.succ m)

-- | Generate 'len' values starting from 'off + len - 1' in descending order.
--
-- >>> descendingSlice (Nat.constant @2) (Nat.constant @3) (Lt.constant @6)
-- [Fin 4,Fin 3,Fin 2]
descendingSlice
  :: forall n off len
  .  Nat off
  -> Nat len
  -> off + len <= n
  -> [Fin n]
{-# inline descendingSlice #-}
descendingSlice !off !len !offPlusLenLteEn =
  go len Lte.reflexive
  where
    go :: Nat m -> m <= len -> [Fin n]
    go !m !mLteEn = case Nat.monus m Nat.one of
      Nothing -> []
      Just (Difference (mpred :: Nat c) cPlusOneEqEm) ->
        let !cLtLen = Lt.transitiveNonstrictR
              (Lt.substituteR (Plus.commutative @1 @c) (Lt.plus Lt.zero Lte.reflexive))
              -- c < c + 1
              (Lte.substituteL (Eq.symmetric cPlusOneEqEm) mLteEn)
              -- c + 1 <= len
            !cPlusOffLtEn = Lt.transitiveNonstrictR
              (Lt.substituteR
                (Plus.commutative @len @off)
                (Lt.plus cLtLen (Lte.reflexive @off)))
              -- c + off < off + len
              offPlusLenLteEn
        in Fin (mpred `Nat.plus` off) cPlusOffLtEn : go mpred (Lte.fromStrict cLtLen)

-- | Extract the 'Int' from a 'Fin n'. This is intended to be used
-- at a boundary where a safe interface meets the unsafe primitives
-- on top of which it is built.
demote :: Fin n -> Int
demote (Fin i _) = Nat.demote i
