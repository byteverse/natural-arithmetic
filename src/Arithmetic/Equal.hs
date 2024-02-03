{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module Arithmetic.Equal
  ( symmetric
  , plusR
  , plusL
  , plusR#
  , plusL#
  , lift
  ) where

import Arithmetic.Unsafe (type (:=:) (Eq), type (:=:#) (Eq#))
import GHC.TypeNats (type (+))

symmetric :: (m :=: n) -> (n :=: m)
{-# INLINE symmetric #-}
symmetric Eq = Eq

plusL :: forall c m n. (m :=: n) -> (c + m :=: c + n)
{-# INLINE plusL #-}
plusL Eq = Eq

plusR :: forall c m n. (m :=: n) -> (m + c :=: n + c)
{-# INLINE plusR #-}
plusR Eq = Eq

plusL# :: forall c m n. (m :=:# n) -> (c + m :=:# c + n)
{-# INLINE plusL# #-}
plusL# _ = Eq# (# #)

plusR# :: forall c m n. (m :=:# n) -> (m + c :=:# n + c)
{-# INLINE plusR# #-}
plusR# _ = Eq# (# #)

lift :: (m :=:# n) -> (m :=: n)
{-# INLINE lift #-}
lift _ = Eq
